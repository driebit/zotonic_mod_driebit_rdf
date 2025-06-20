zotonic_mod_driebit_rdf
=======================

A Zotonic module for retrieving, working with, and producing RDF triples, based
on the [RDF 1.2 specification](https://www.w3.org/TR/rdf12-concepts/).

### Table of contents

1. [Features](#features)
   * [Represent resources in RDF](#represent-resources-in-rdf)
   * [Inline JSON-LD](#inline-json-ld)
2. [Notifications](#notifications)

Features
--------

### Represent resources in RDF

Enable `zotonic_mod_driebit_rdf` in Zotonic, then request any page with content
negotiation, using the proper Accept header to get an RDF representation of the
resource at its URI (where `<id>` is the id of some resource):
```bash
curl -L -H Accept:application/ld+json https://yoursite.com/id/<id>
curl -L -H Accept:text/turtle https://yoursite.com/id/<id>
```
Both of these use the default ontology, [schema.org](https://schema.org/), and
are also available (e.g. for viewing in the browser) on separate endpoints:
```bash
curl -L http://yoursite.com/rdf/json_ld/<id>
curl -L http://yoursite.com/rdf/turtle/<id>
```

Additionally, the module supports custom ontologies and representations (see
below), whose result can be obtained with the generic endpoints:
```bash
curl -L http://yoursite.com/rdf/<ser>/<ont>/<id>
curl -L http://yoursite.com/rdf/<ser>/<id>?ontology=<ont>
curl -L http://yoursite.com/rdf/<id>?ontology=<ont>&serialization=<ser>
```
(where `<ser>` and `<ont>` are the name of the serialization and ontology used
and the `?ontology` query parameter can be specified multiple times).

#### Export rules

The RDF export follows some rules.

- ACL: regardless of ontologies and serialization:
  - users can only see the RDF representation of resources that they can view.
    Otherwise, they receive a `403` "forbidden" error code.
  - the RDF representation is calculated from the fields and edges visible to
    the current user, any other is excluded.
- CORS headers: `controller_rdf` ignores configuration to ease sharing content
  and allows any origin in its [Access-Control-Allow-Origin header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Access-Control-Allow-Origin).
- Ontologies and serializations are separate steps in making an RDF representation:
  - supported ontologies produce an [RDF Graph](https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-graph)
    without needing to know how they'll be serialized.
  - supported serializations produce a [concrete representation](https://www.w3.org/TR/rdf12-concepts/#dfn-concrete-rdf-syntax)
    starting from an _RDF Graph_ without needing to know which ontologies were used.
- The default representations use the [Schema.org ontology](https://schema.org/),
  as [recommended by NDE](https://netwerk-digitaal-erfgoed.github.io/cm-implementation-guidelines/#generic-data-model) and either the [Turtle](https://www.w3.org/TR/rdf12-turtle/)
  or [JSON-LD](https://www.w3.org/TR/json-ld11/) serialization.

### Inline JSON-LD

To enable rich search results,
[Google recommends](https://developers.google.com/search/docs/advanced/structured-data/intro-structured-data#structured-data-format)
embedded JSON-LD.

This module [includes an embedded JSON-LD snippet](priv/templates/rdf/resource.tpl):
```html
<script type="application/ld+json">
{
    "@id": "https://example.com/id/380",
    "@type": ...
    ...
}
</script>
```
which is included automatically in the `head` of every resource page if the site
use a base template with an `{% all include "_html_head.tpl" %}`, as it's the
case for `zotonic_mod_base`.

The embedded representation is identical to the exported one and you can test
your pages using Google’s [Rich Result Test](https://search.google.com/test/rich-results).

Notifications
-------------

This module uses [Zotonic Notifications](https://zotonic.com/docs/1274/notifications)
which can be used to change or expand the supported ontologies and/or serializations.

The types for RDF structures and the notifications used here can all be found in
the [`driebit_rdf` header file](include/driebit_rdf.hrl) and included with:
```erlang
-include("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").
```

### Changing the RDF graph of a resource

The first step to represent a resource with RDF is to calculate its [RDF Graph](https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-graph).

For this purpose, `mod_driebit_rdf` uses an `rsc_to_rdf_graph` notification in
search of a module that implements each of the requested ontologies for that
resource, also passing the resource category for convenience.

This can be observed by other modules to decide how to build an RDF graph from scratch:
```erlang
-export([
    observe_rsc_to_rdf_graph/3
]).

-include("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

observe_rsc_to_rdf_graph(#rsc_to_rdf_graph{rsc_id = RscId, category = Category, ontology = Ontology},Context) ->
    RdfGraph = sets:new();
    % ... code to build the AST of the graph here ...
    {ok, RdfGraph};
observe_rsc_to_rdf_graph(#rsc_to_rdf_graph{}, _Context) ->
    undefined.
```
however, if nobody picks this notification up, `mod_driebit_rdf` has its own
observer, implementing the default logic for building an RDF graph: the set of
all the triples built from every (visible) field and edge connected to the resource.

For ease of customization, these too are notifications, `triple_to_rdf` which provide:
- `rsc_id`
- `category` of the resource
- `link_type` which can be `property`, `outgoing_edge` or `incoming_edge`
- `link_name` which is the name of the field or predicate used, depending on `link_type`
- `value` which is either the ID of the resource linked by edge or the value of the field, depending on `link_type`

Observing these notifications allows to override or implement part of an ontology
without having to reimplement all of it, e.g.
```erlang
-export([
    observe_triple_to_rdf/3
]).

-include("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

% support a non-standard category by giving it a schema.org type:
observe_triple_to_rdf(
    #triple_to_rdf{
        rsc_id = RscId,
        category = remark,
        link_type = property,
        link_name = <<"id">>,
        value = RscId,
        ontology = schema_org
    },
    Context
) ->
    {ok, rdf_schema_org:type_triple(RscId, <<"Comment">>, Context)};
% add a new property to the RDF graph of articles from a new predicate:
observe_triple_to_rdf(
    #triple_to_rdf{
        rsc_id = RscId,
        category = article,
        link_type = outgoing_edge,
        link_name = <<"has_comment">>,
        value = CommentId,
        ontology = schema_org
    },
    Context
) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        rdf_schema_org:namespace_iri(comment),
        ObjectId,
        Context
    )};
% don't forget to return 'undefined' otherwise, so that others can pick this up:
observe_triple_to_rdf(#triple_to_rdf{}, _Context) ->
    undefined.
```

Note: the `rdf_utils` module makes creating triples much easier, see also its
usage in `rdf_schema_org` to see examples.

### Changing the concrete representation of an RDF graph

After an RDF graph has been obtained from a resource, this needs to be serialized
by a [concrete syntax](https://www.w3.org/TR/rdf12-concepts/#dfn-concrete-rdf-syntax).

Another notification, `serialize_rdf` is used to do this, simply carrying the
graph to serialize and the name of the serialization to be used.

This module handles the ones for `json_ld` and `turtle` already, but any other
module can override these or add more by observing the notification and returning
a `binary` result string:
```erlang
-export([
    observe_serialize_rdf/3
]).

-include("zotonic_mod_driebit_rdf/include/driebit_rdf.hrl").

observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = trig}, Context) ->
    Result = <<"">>,
    % ... code to serialize the graph to TriG ...
    {ok, Result};
observe_serialize_rdf(#serialize_rdf{}, _Context) ->
    undefined.
```
