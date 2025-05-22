%% @author Driebit
%% @copyright 2025 Driebit
%% @end

%% DATATYPES

-type iri() :: binary().

-record(rdf_literal, {
    lexical_form :: binary(),
    datatype_iri :: iri(),
    language_tag :: undefined | binary(),
    base_direction :: undefined | ltr | rtl
}).

-record(rdf_triple, {
    subject :: iri() | blank,
    predicate :: iri(),
    object :: iri() | blank | #rdf_literal{} | #rdf_triple{}
}).

-type rdf_graph() :: sets:set(#rdf_triple{}).

%% NOTIFICATIONS

-record(rsc_to_rdf_graph, {
    rsc_id :: m_rsc:resource_id(),
    category :: atom(),
    ontology :: atom()
}).

-record(triple_to_rdf, {
    rsc_id :: m_rsc:resource_id(),
    category :: atom(),
    link_type :: property | outgoing_edge | incoming_edge,
    link_name :: binary(),
    value :: term() | m_rsc:resource_id(),
    ontology :: atom()
}).

-record(serialize_rdf, {
    rdf_graph :: rdf_graph(),
    serialization :: atom()
}).
