%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(rdf_schema_org).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    triple_to_rdf/2,

    property_to_rdf/5,
    outedge_to_rdf/5,
    inedge_to_rdf/5,

    namespace_iri/1,
    type_triple/3
]).


-spec triple_to_rdf(#triple_to_rdf{}, z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
triple_to_rdf(#triple_to_rdf{link_type = property, ontology = schema_org} = TripleToRdf, Context) ->
    property_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(#triple_to_rdf{link_type = outgoing_edge, ontology = schema_org} = TripleToRdf, Context) ->
    outedge_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(#triple_to_rdf{link_type = incoming_edge, ontology = schema_org} = TripleToRdf, Context) ->
    inedge_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.

-spec property_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
property_to_rdf(_RscId, _Category, _PropName, undefined, _Context) ->
    % Ignore empty values
    undefined;

% The ID property doesn't produce any triple, but is always there,
% so we take this opportunity to determine the resource type instead:
property_to_rdf(RscId, person, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Person">>, Context)};
property_to_rdf(RscId, institution, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Organization">>, Context)};
property_to_rdf(RscId, institute, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Organization">>, Context)};
property_to_rdf(RscId, organization, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Organization">>, Context)};
property_to_rdf(RscId, text, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Article">>, Context)};
property_to_rdf(RscId, article, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Article">>, Context)};
property_to_rdf(RscId, news, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"NewsArticle">>, Context)};
property_to_rdf(RscId, artifact, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"CreativeWork">>, Context)};
property_to_rdf(RscId, collection, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Collection">>, Context)};
property_to_rdf(RscId, media, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"MediaObject">>, Context)};
property_to_rdf(RscId, event, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Event">>, Context)};
property_to_rdf(RscId, location, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Place">>, Context)};
property_to_rdf(RscId, keyword, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"DefinedTerm">>, Context)};

property_to_rdf(RscId, text, <<"body">>, Value, Context) ->
    {ok,
        [
            rdf_utils:value_triple(
                RscId,
                namespace_iri(articleBody),
                Value,
                Context
            ),
            rdf_utils:value_triple(
                RscId,
                namespace_iri(encodingFormat),
                <<"text/html">>,
                Context
            )
        ]
    };
property_to_rdf(RscId, _Category, <<"created">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(dateCreated),
        Value,
        Context
    )};
property_to_rdf(RscId, event, <<"date_start">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(startDate),
        Value,
        Context
    )};
property_to_rdf(RscId, event, <<"date_end">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(endDate),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"birth_city">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(birthPlace), namespace_iri(address), namespace_iri(addressLocality)],
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"date_start">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(birthDate),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"date_end">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(deathDate),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"name_first">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(givenName),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"name_surname">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(familyName),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"email">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(email),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"license">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(license),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"modified">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(dateModified),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"phone">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(telephone),
        Value,
        Context
    )};
property_to_rdf(RscId, text, <<"publication_start">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(datePublished),
        Value,
        Context
    )};
property_to_rdf(RscId, artifact, <<"publication_start">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(datePublished),
        Value,
        Context
    )};
property_to_rdf(RscId, text, <<"subtitle">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(alternativeHeadline),
        Value,
        Context
    )};
property_to_rdf(RscId, artifact, <<"subtitle">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(alternativeHeadline),
        Value,
        Context
    )};
property_to_rdf(RscId, media, <<"subtitle">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(caption),
        Value,
        Context
    )};
property_to_rdf(RscId, _Category, <<"summary">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(description),
        Value,
        Context
    )};
property_to_rdf(RscId, text, <<"title">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(headline),
        Value,
        Context
    )};
property_to_rdf(RscId, artifact, <<"title">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(headline),
        Value,
        Context
    )};
property_to_rdf(RscId, collection, <<"title">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(headline),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"title">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(name),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"subtitle">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(alternateName),
        Value,
        Context
    )};
property_to_rdf(RscId, keyword, <<"title">>, Value, Context) ->
    {ok,
        [
            rdf_utils:value_triple(
                RscId,
                namespace_iri(name),
                Value,
                Context
            ),
            rdf_utils:value_triple(
                RscId,
                namespace_iri(termCode),
                Value,
                Context
            )
        ]
    };
property_to_rdf(RscId, _Category, <<"website">>, Value, Context) ->
    {ok, rdf_utils:value_triple(
        RscId,
        namespace_iri(url),
        Value,
        Context
    )};

property_to_rdf(RscId, query, <<"query">>, _Value, Context) ->
    % Stored search query: consider each result a "part" of the collection
    SearchResult = z_search:search(
        <<"query">>,
        #{<<"query_id">> => RscId},
        1,
        undefined,
        #{},
        Context
    ),
    ResultTriples = lists:map(
        fun (ResId) ->
            rdf_utils:resolve_triple(
                RscId,
                namespace_iri(hasPart),
                ResId,
                Context
            )
        end,
        SearchResult#search_result.result
    ),
    {ok, ResultTriples};

property_to_rdf(RscId, address, <<"address_city">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(address), namespace_iri(addressLocality)],
        Value,
        Context
    )};
property_to_rdf(RscId, address, <<"address_postcode">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(address), namespace_iri(postalCode)],
        Value,
        Context
    )};
property_to_rdf(RscId, address, <<"address_street_1">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(address), namespace_iri(streetAddress)],
        Value,
        Context
    )};
property_to_rdf(RscId, address, <<"address_country">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(address), namespace_iri(addressCountry)],
        Value,
        Context
    )};

property_to_rdf(RscId, address, <<"pivot_location_lat">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(geo), namespace_iri(latitude)],
        Value,
        Context
    )};
property_to_rdf(RscId, address, <<"pivot_location_lng">>, Value, Context) ->
    {ok, rdf_utils:nested_triple(
        RscId,
        [namespace_iri(geo), namespace_iri(longitude)],
        Value,
        Context
    )};
property_to_rdf(RscId, Category, PropName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            property_to_rdf(RscId, ParentCat, PropName, Value, Context)
    end.

-spec outedge_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
outedge_to_rdf(RscId, text, <<"about">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(about),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, artifact, <<"about">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(about),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, collection, <<"about">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(about),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, event, <<"about">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(about),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, text, <<"author">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(author),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, artifact, <<"author">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(author),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, collection, <<"author">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(author),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, _Category, <<"depiction">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(image),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, _Category, <<"subject">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(keywords),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, text, <<"haspart">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(hasPart),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, artifact, <<"haspart">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(hasPart),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, collection, <<"haspart">>, ObjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(hasPart),
        ObjectId,
        Context
    )};
outedge_to_rdf(RscId, Category, PredName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            outedge_to_rdf(RscId, ParentCat, PredName, Value, Context)
    end.

-spec inedge_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
inedge_to_rdf(RscId, _Category, <<"about">>, SubjectId, Context) ->
    {ok, rdf_utils:resolve_triple(
        RscId,
        namespace_iri(subjectOf),
        SubjectId,
        Context
    )};
inedge_to_rdf(RscId, Category, PredName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            inedge_to_rdf(RscId, ParentCat, PredName, Value, Context)
    end.

-spec namespace_iri(atom() | binary()) -> iri().
namespace_iri(TermName) when is_atom(TermName) ->
    namespace_iri(z_convert:to_binary(TermName));
namespace_iri(TermName) when is_binary(TermName) ->
    <<"http://www.w3.org/2001/XMLSchema#", TermName/binary>>.

-spec type_triple(m_rsc:resource(), iri(), z:context()) -> #rdf_triple{}.
type_triple(RscId, SchemaType, Context) ->
    #rdf_triple{
        subject = rdf_utils:resolve_iri(RscId, Context),
        predicate = rdf_xsd:rdf_namespace_iri(type),
        object = namespace_iri(SchemaType)
    }.
