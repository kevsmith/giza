%% Copyright (c) 2009 Electronic Arts, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

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
         filters=[],
         ranker}).
