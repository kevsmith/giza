## Description
giza is a client library for the Sphinx search engine (http://www.sphinxsearch.com). It uses the binary
protocol spoken by the searchd daemon. giza doesn't use the MySQL protocol support built into Sphinx.

giza currently supports the following features:

- Full text (Unicode) searching
- Pagination via query limit and offset
- Document id ranges via min_id and max_id

*Note: All text is represented as Erlang binaries*

## Examples

1. Performing a simple search:

`Query = giza_query:new("users", "frederickson"),`
`Results = giza_request:send(Query)`

2. Performing a paginated search:

`Q = giza_query:new("users", "frederickson"),`
`Q1 = giza_query:offset(Q, 10),`
`Results = giza_query:send(Query)`

3. Querying a non-default host:
`Q = giza_query:new("users", "frederickson"),`
`Q1 = giza_query:host(Q, "search.somewhere"),`
`Results = giza_query:send(Query)`