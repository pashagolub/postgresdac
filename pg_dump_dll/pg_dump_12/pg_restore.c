/*-------------------------------------------------------------------------
 *
 * pg_restore.c
 *	pg_restore is an utility extracting postgres database definitions
 *	from a backup archive created by pg_dump using the archiver
 *	interface.
 *
 *	pg_restore will read the backup archive and
 *	dump out a script that reproduces
 *	the schema of the database in terms of
 *		  user-defined types
 *		  user-defined functions
 *		  tables
 *		  indexes
 *		  aggregates
 *		  operators
 *		  ACL - grant/revoke
 *
 * the output script is SQL that is understood by PostgreSQL
 *
 * Basic process in a restore operation is:
 *
 *	Open the Archive and read the TOC.
 *	Set flags in TOC entries, and *maybe* reorder them.
 *	Generate script to stdout
 *	Exit
 *
 * Copyright (c) 2000, Philip Warner
 *		Rights are granted to use this software in any way so long
 *		as this notice is not removed.
 *
 *	The author is not responsible for loss or damages that may
 *	result from its use.
 *
 *
 * IDENTIFICATION
 *		src/bin/pg_dump/pg_restore.c
 *
 *-------------------------------------------------------------------------
 */
#include "postgres_fe.h"

#include <ctype.h>
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif

#include "getopt_long.h"

#include "dumputils.h"
#include "parallel.h"
#include "pg_backup_utils.h"

//mi
#include "mi_global_pwd.h"

static void usage(const char *progname);

typedef struct option optType;

int
main(int argc, char **argv)
{
	RestoreOptions *opts;
	int			c;
	int			exit_code;
	int			numWorkers = 1;
	Archive    *AH;
	char	   *inputFileSpec;
	static int	disable_triggers = 0;
	static int	enable_row_security = 0;
	static int	if_exists = 0;
	static int	no_data_for_failed_tables = 0;
	static int	outputNoTablespaces = 0;
	static int	use_setsessauth = 0;
	static int	no_comments = 0;
	static int	no_publications = 0;
	static int	no_security_labels = 0;
	static int	no_subscriptions = 0;
	static int	strict_names = 0;

	struct option cmdopts[] = {
		{"clean", 0, NULL, 'c'},
		{"create", 0, NULL, 'C'},
		{"data-only", 0, NULL, 'a'},
		{"dbname", 1, NULL, 'd'},
		{"exit-on-error", 0, NULL, 'e'},
		{"exclude-schema", 1, NULL, 'N'},
		{"file", 1, NULL, 'f'},
		{"format", 1, NULL, 'F'},
		{"function", 1, NULL, 'P'},
		{"host", 1, NULL, 'h'},
		{"index", 1, NULL, 'I'},
		{"jobs", 1, NULL, 'j'},
		{"list", 0, NULL, 'l'},
		{"no-privileges", 0, NULL, 'x'},
		{"no-acl", 0, NULL, 'x'},
		{"no-owner", 0, NULL, 'O'},
		{"no-reconnect", 0, NULL, 'R'},
		{"port", 1, NULL, 'p'},
		{"no-password", 0, NULL, 'w'},
		{"password", 0, NULL, 'W'},
		{"schema", 1, NULL, 'n'},
		{"schema-only", 0, NULL, 's'},
		{"superuser", 1, NULL, 'S'},
		{"table", 1, NULL, 't'},
		{"trigger", 1, NULL, 'T'},
		{"use-list", 1, NULL, 'L'},
		{"username", 1, NULL, 'U'},
		{"verbose", 0, NULL, 'v'},
		{"single-transaction", 0, NULL, '1'},

		/*
		 * the following options don't have an equivalent short option letter
		 */
		{"disable-triggers", no_argument, &disable_triggers, 1},
		{"enable-row-security", no_argument, &enable_row_security, 1},
		{"if-exists", no_argument, &if_exists, 1},
		{"no-data-for-failed-tables", no_argument, &no_data_for_failed_tables, 1},
		{"no-tablespaces", no_argument, &outputNoTablespaces, 1},
		{"role", required_argument, NULL, 2},
		{"section", required_argument, NULL, 3},
		{"strict-names", no_argument, &strict_names, 1},
		{"use-set-session-authorization", no_argument, &use_setsessauth, 1},
		{"no-comments", no_argument, &no_comments, 1},
		{"no-publications", no_argument, &no_publications, 1},
		{"no-security-labels", no_argument, &no_security_labels, 1},
		{"no-subscriptions", no_argument, &no_subscriptions, 1},

		{NULL, 0, NULL, 0}
	};

	pg_logging_init(argv[0]);
	pg_logging_set_level(PG_LOG_WARNING);
	set_pglocale_pgservice(argv[0], PG_TEXTDOMAIN("pg_dump"));

	init_parallel_dump_utils();

	opts = NewRestoreOptions();

	progname = get_progname(argv[0]);

	if (argc > 1)
	{
		if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-?") == 0)
		{
			usage(progname);
			exit_nicely(0);
		}
		if (strcmp(argv[1], "--version") == 0 || strcmp(argv[1], "-V") == 0)
		{
			puts("pg_restore (PostgreSQL) " PG_VERSION);
			exit_nicely(0);
		}
	}

	while ((c = getopt_long(argc, argv, "acCd:ef:F:h:I:j:lL:n:N:Op:P:RsS:t:T:U:vwWx1",
							cmdopts, NULL)) != -1)
	{
		switch (c)
		{
			case 'a':			/* Dump data only */
				opts->dataOnly = 1;
				break;
			case 'c':			/* clean (i.e., drop) schema prior to create */
				opts->dropSchema = 1;
				break;
			case 'C':
				opts->createDB = 1;
				break;
			case 'd':
				opts->dbname = pg_strdup(optarg);
				break;
			case 'e':
				opts->exit_on_error = true;
				break;
			case 'f':			/* output file name */
				opts->filename = pg_strdup(optarg);
				break;
			case 'F':
				if (strlen(optarg) != 0)
					opts->formatName = pg_strdup(optarg);
				break;
			case 'h':
				if (strlen(optarg) != 0)
					opts->pghost = pg_strdup(optarg);
				break;

			case 'j':			/* number of restore jobs */
				numWorkers = atoi(optarg);
				break;

			case 'l':			/* Dump the TOC summary */
				opts->tocSummary = 1;
				break;

			case 'L':			/* input TOC summary file name */
				opts->tocFile = pg_strdup(optarg);
				break;

			case 'n':			/* Dump data for this schema only */
				simple_string_list_append(&opts->schemaNames, optarg);
				break;

			case 'N':			/* Do not dump data for this schema */
				simple_string_list_append(&opts->schemaExcludeNames, optarg);
				break;

			case 'O':
				opts->noOwner = 1;
				break;

			case 'p':
				if (strlen(optarg) != 0)
					opts->pgport = pg_strdup(optarg);
				break;
			case 'R':
				/* no-op, still accepted for backwards compatibility */
				break;
			case 'P':			/* Function */
				opts->selTypes = 1;
				opts->selFunction = 1;
				simple_string_list_append(&opts->functionNames, optarg);
				break;
			case 'I':			/* Index */
				opts->selTypes = 1;
				opts->selIndex = 1;
				simple_string_list_append(&opts->indexNames, optarg);
				break;
			case 'T':			/* Trigger */
				opts->selTypes = 1;
				opts->selTrigger = 1;
				simple_string_list_append(&opts->triggerNames, optarg);
				break;
			case 's':			/* dump schema only */
				opts->schemaOnly = 1;
				break;
			case 'S':			/* Superuser username */
				if (strlen(optarg) != 0)
					opts->superuser = pg_strdup(optarg);
				break;
			case 't':			/* Dump specified table(s) only */
				opts->selTypes = 1;
				opts->selTable = 1;
				simple_string_list_append(&opts->tableNames, optarg);
				break;

			case 'U':
				opts->username = pg_strdup(optarg);
				break;

			case 'v':			/* verbose */
				opts->verbose = 1;
				pg_logging_set_level(PG_LOG_INFO);
				break;

			case 'w':
				opts->promptPassword = TRI_NO;
				break;

			case 'W':
				opts->promptPassword = TRI_YES;
				break;

			case 'x':			/* skip ACL dump */
				opts->aclsSkip = 1;
				break;

			case '1':			/* Restore data in a single transaction */
				opts->single_txn = true;
				opts->exit_on_error = true;
				break;

			case 0:

				/*
				 * This covers the long options without a short equivalent.
				 */
				break;

			case 2:				/* SET ROLE */
				opts->use_role = pg_strdup(optarg);
				break;

			case 3:				/* section */
				set_dump_section(optarg, &(opts->dumpSections));
				break;

			default:
				fprintf(stderr, _("Try \"%s --help\" for more information.\n"), progname);
				exit_nicely(1);
		}
	}

	/* Get file name from command line */
	if (optind < argc)
		inputFileSpec = argv[optind++];
	else
		inputFileSpec = NULL;

	/* Complain if any arguments remain */
	if (optind < argc)
	{
		pg_log_error("too many command-line arguments (first is \"%s\")",
					 argv[optind]);
		fprintf(stderr, _("Try \"%s --help\" for more information.\n"),
				progname);
		exit_nicely(1);
	}

	/* Complain if neither -f nor -d was specified (except if dumping TOC) */
	if (!opts->dbname && !opts->filename && !opts->tocSummary)
	{
		pg_log_error("one of -d/--dbname and -f/--file must be specified");
		exit_nicely(1);
	}

	/* Should get at most one of -d and -f, else user is confused */
	if (opts->dbname)
	{
		if (opts->filename)
		{
			pg_log_error("options -d/--dbname and -f/--file cannot be used together");
			fprintf(stderr, _("Try \"%s --help\" for more information.\n"),
					progname);
			exit_nicely(1);
		}
		opts->useDB = 1;
	}

	if (opts->dataOnly && opts->schemaOnly)
	{
		pg_log_error("options -s/--schema-only and -a/--data-only cannot be used together");
		exit_nicely(1);
	}

	if (opts->dataOnly && opts->dropSchema)
	{
		pg_log_error("options -c/--clean and -a/--data-only cannot be used together");
		exit_nicely(1);
	}

	/*
	 * -C is not compatible with -1, because we can't create a database inside
	 * a transaction block.
	 */
	if (opts->createDB && opts->single_txn)
	{
		pg_log_error("options -C/--create and -1/--single-transaction cannot be used together");
		exit_nicely(1);
	}

	if (numWorkers <= 0)
	{
		pg_log_error("invalid number of parallel jobs");
		exit(1);
	}

	/* See comments in pg_dump.c */
#ifdef WIN32
	if (numWorkers > MAXIMUM_WAIT_OBJECTS)
	{
		pg_log_error("maximum number of parallel jobs is %d",
					 MAXIMUM_WAIT_OBJECTS);
		exit(1);
	}
#endif

	/* Can't do single-txn mode with multiple connections */
	if (opts->single_txn && numWorkers > 1)
	{
		pg_log_error("cannot specify both --single-transaction and multiple jobs");
		exit_nicely(1);
	}

	opts->disable_triggers = disable_triggers;
	opts->enable_row_security = enable_row_security;
	opts->noDataForFailedTables = no_data_for_failed_tables;
	opts->noTablespace = outputNoTablespaces;
	opts->use_setsessauth = use_setsessauth;
	opts->no_comments = no_comments;
	opts->no_publications = no_publications;
	opts->no_security_labels = no_security_labels;
	opts->no_subscriptions = no_subscriptions;

	if (if_exists && !opts->dropSchema)
	{
		pg_log_error("option --if-exists requires option -c/--clean");
		exit_nicely(1);
	}
	opts->if_exists = if_exists;
	opts->strict_names = strict_names;

	if (opts->formatName)
	{
		switch (opts->formatName[0])
		{
			case 'c':
			case 'C':
				opts->format = archCustom;
				break;

			case 'd':
			case 'D':
				opts->format = archDirectory;
				break;

			case 't':
			case 'T':
				opts->format = archTar;
				break;

			default:
				pg_log_error("unrecognized archive format \"%s\"; please specify \"c\", \"d\", or \"t\"",
							 opts->formatName);
				exit_nicely(1);
		}
	}

	AH = OpenArchive(inputFileSpec, opts->format);

	SetArchiveOptions(AH, NULL, opts);

	/*
	 * We don't have a connection yet but that doesn't matter. The connection
	 * is initialized to NULL and if we terminate through exit_nicely() while
	 * it's still NULL, the cleanup function will just be a no-op.
	 */
	on_exit_close_archive(AH);

	/* Let the archiver know how noisy to be */
	AH->verbose = opts->verbose;

	/*
	 * Whether to keep submitting sql commands as "pg_restore ... | psql ... "
	 */
	AH->exit_on_error = opts->exit_on_error;

	if (opts->tocFile)
		SortTocFromFile(AH);

	AH->numWorkers = numWorkers;

	if (opts->tocSummary)
		PrintTOCSummary(AH);
	else
	{
		ProcessArchiveRestoreOptions(AH);
		RestoreArchive(AH);
	}

	/* done, print a summary of ignored errors */
	if (AH->n_errors)
		pg_log_warning("errors ignored on restore: %d", AH->n_errors);

	/* AH may be freed in CloseArchive? */
	exit_code = AH->n_errors ? 1 : 0;

	CloseArchive(AH);

	return exit_code;
}

static void
usage(const char *progname)
{
}

//mi
#include "pg_restore_dll.h"