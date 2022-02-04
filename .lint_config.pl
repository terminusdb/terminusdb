% We have to find out about those
ignore_predicate("dateTimeStamp/11").
ignore_predicate("time/8").
ignore_predicate("dateTime/11").
ignore_predicate("embellish_flag/3").
ignore_predicate("sp_card/4"). % We probably need to add it to terminus-store docs
ignore_predicate("woql_compile:min_member/3").
ignore_predicate("woql_compile/2").
% this one is imported, but fails to be recognized. I think xref parses the pldocs of
% prolog/terminus_store.pl. And unfortunately, layer_to_id/2 is not in the pldocs.
ignore_predicate("layer_to_id/2").
ignore_predicate("'document/patch'/2").
ignore_predicate("'document/patch:min_member'/2").
ignore_predicate("util:mapm/5").
ignore_predicate("util/4").
