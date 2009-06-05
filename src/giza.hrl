-define(EMPTY_STRING, <<>>).

% Searchd commands
-define(SPHINX_COMMAND_SEARCH, 0).
-define(SPHINX_COMMAND_EXCERPT, 1).
-define(SPHINX_COMMAND_UPDATE, 2).
-define(SPHINX_COMMAND_KEYWORDS, 3).

% Querying
-define(SPHINX_MATCH_ALL, 0).
-define(SPHINX_MATCH_ANY, 1).
-define(SPHINX_MATCH_PHRASE, 2).
-define(SPHINX_MATCH_BOOLEAN, 3).
-define(SPHINX_MATCH_EXTENDED, 4).
-define(SPHINX_MATCH_FULLSCAN, 5).
-define(SPHINX_MATCH_EXTENDED2, 6).


-record(giza_query,
        {host="localhost",
         port=3312,
         command,
         command_version,
         index=?EMPTY_STRING,
         offset=0,
         limit=25,
         min_id=0,
         max_id=0,
         mode,
         sort,
         sort_by=?EMPTY_STRING,
         group_by=?EMPTY_STRING,
         group_fun,
         group_sort,
         query_string=?EMPTY_STRING,
         ranker}).
