## Description
giza is a client library for the Sphinx search engine (http://www.sphinxsearch.com). It speaks Sphinx's
binary searchd protocol natively.

giza currently supports the following features:

- Full text (Unicode) searching
- Attribute filters (numeric values only)
- Index updates
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
4. Using giza's attribute filtering:
<pre>
    Q = giza_query:new("users", "frederickson"),
    %% This is an inclusionary filter by default
    Q1 = giza_query:add_filter(Q, "user_type", [1,3,5]),
    Results = giza_request:send(Q1)
</pre>
<pre>
    Q = giza_query:new("users", "frederickson"),
    %% Filter with explicit exclude info (exclude == true)
    Q1 = giza_query:add_filter(Q, "user_type", true, [1,3,5]),
    Results = giza_request:send(Q1)
</pre>
5. Updating a Sphinx index using giza:
<pre>
  U = giza_update:new("users"),
  U1 = giza_update:add_field(U, "user_type"),
  U2 = giza_update:add_doc(U1, 12345, [1]),
  %% Returns number of docs updated
  giza_request:send(U2),
</pre>

Thanks to Nick Gerakines and the folks at EA for supporting giza!