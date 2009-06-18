## Description
giza is a client library for the Sphinx search engine (http://www.sphinxsearch.com). It speaks Sphinx's
binary searchd protocol natively.

giza currently supports the following features:

- Full text (Unicode) searching
- Pagination via query limit and offset
- Document id ranges via min_id and max_id

Coming soon:

- Excerpt support
- Support for more query fields
- Generic Sphinx test harness

## Examples

1. Performing a simple search:
<pre>
    Query = giza_query:new("users", "frederickson"),
    Results = giza_request:send(Query)
</pre>
2. Performing a paginated search:
<pre>
    Q = giza_query:new("users", "frederickson"),
    Q1 = giza_query:offset(Q, 10),
    Results = giza_request:send(Q1)
</pre>
3. Querying a non-default host:
<pre>
    Q = giza_query:new("users", "frederickson"),
    Q1 = giza_query:host(Q, "search.somewhere"),
    Results = giza_request:send(Q1)
</pre>
4. Using giza's attribute filtering (Inclusive only and numeric support right now):
<pre>
    Q = giza_query:new("users", "frederickson"),
    Q1 = giza_query:add_filter(Q, "user_type", [1,3,5]),
    Results = giza_request:send(Q1)
</pre>

Thanks to Nick Gerakines and the folks at EA for supporting giza!