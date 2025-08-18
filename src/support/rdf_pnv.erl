%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for the Person Name Vocabulary:
%% https://www.lodewijkpetram.nl/vocab/pnv/doc/ (aka https://w3id.org/pnv)
%% @end

-module(rdf_pnv).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    triple_to_rdf/2,

    property_to_rdf/5,

    namespace_iri/0,
    namespaced_iri/1,
    type_triple/3
]).

-spec triple_to_rdf(#triple_to_rdf{}, z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
triple_to_rdf(#triple_to_rdf{link_type = property, ontology = pnv} = TripleToRdf, Context) ->
    property_to_rdf(
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

% Usually the title for 'person' is their full name, but that's not always the
% case, so we re-calculate it. Moreover, this is necessary because:
% > The pnv:literalName property is an exception. This property may only be left
% > blank if a person's name is unknown or if a person was unnamed (e.g. a child
% > that died shortly after being born).
property_to_rdf(RscId, person, <<"title">>, _Value, Context) ->
    NamePrefix = m_rsc:p(RscId, <<"name_prefix">>, <<"">>, Context),
    NameFirst = m_rsc:p(RscId, <<"name_first">>, <<"">>, Context),
    FullFirstName = z_string:trim(<<NamePrefix/binary, " ", NameFirst/binary>>),

    SurnamePrefix = m_rsc:p(RscId, <<"name_surname_prefix">>, <<"">>, Context),
    Surname = m_rsc:p(RscId, <<"name_surname">>, <<"">>, Context),
    FullSurname = z_string:trim(<<SurnamePrefix/binary, " ", Surname/binary>>),

    case z_string:trim(<<FullFirstName/binary, " ", FullSurname/binary>>) of
        <<"">> ->
            undefined;
        FullName ->
            {ok, hasname_triples(
                RscId,
                namespaced_iri(literalName),
                FullName,
                Context
            )}
    end;
property_to_rdf(RscId, person, <<"name_prefix">>, Value, Context) ->
    {ok, hasname_triples(
        RscId,
        namespaced_iri(prefix),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"name_first">>, Value, Context) ->
    {ok, hasname_triples(
        RscId,
        namespaced_iri(givenName),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"name_surname_prefix">>, Value, Context) ->
    {ok, hasname_triples(
        RscId,
        namespaced_iri(surnamePrefix),
        Value,
        Context
    )};
property_to_rdf(RscId, person, <<"name_surname">>, Value, Context) ->
    {ok, hasname_triples(
        RscId,
        namespaced_iri(baseSurname),
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

-spec namespace_iri() -> iri().
namespace_iri() ->
    <<"https://w3id.org/pnv#">>.

-spec namespaced_iri(atom() | binary()) -> iri().
namespaced_iri(TermName) when is_atom(TermName) ->
    namespaced_iri(z_convert:to_binary(TermName));
namespaced_iri(TermName) when is_binary(TermName) ->
    <<(namespace_iri())/binary, TermName/binary>>.

-spec type_triple(m_rsc:resource(), iri(), z:context()) -> #rdf_triple{}.
type_triple(RscId, PvnType, Context) ->
    #rdf_triple{
        subject = rdf_utils:resolve_iri(RscId, Context),
        predicate = rdf_xsd:rdf_namespaced_iri(type),
        object = namespaced_iri(PvnType)
    }.

% Helper function to add a nested triple to the rsc's 'PersonName'
hasname_triples(RscId, Predicate, Object, Context) ->
    [
        #rdf_triple{
            subject = rdf_utils:resolve_iri(RscId, Context),
            predicate = namespaced_iri(hasName),
            object = type_triple(undefined, <<"PersonName">>, Context)
        },
        #rdf_triple{
            subject = rdf_utils:resolve_iri(RscId, Context),
            predicate = namespaced_iri(hasName),
            object = rdf_utils:value_triple(undefined, Predicate, Object, Context)
        }
    ].
