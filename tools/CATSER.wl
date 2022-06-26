(* ::Package:: *)

BeginPackage["CATSER`"]


Names@"`*"


Quiet[Remove@"`*", {Remove::rmnsm}]


GetTagNames
ShortenTagName
QueryTagFullName
NestedRuleToList
GenerateShortenDictionary
QueryByHeadSubsequence


Begin["`Private`"]


Quiet[Remove@"`*", {Remove::rmnsm}]


assignKey[k_][v:{(_Rule|_String)..}] := assignKey[k]/@v//Splice


assignKey[k_][v:_Rule|_String] := k->v


handleAssociation[a_Association] := KeyValueMap[handleKeyValue]@a


handleAssociation[s_String] := s


handleKeyValue[k_,v:{(_Association|_String)..}] := assignKey[k]@handleAssociation@#&/@v//Splice


handleKeyValue[k_,v:{__String}] := assignKey[k]@v


GetTagNames[tagSystem_] := handleKeyValue[tagSystem["\:6807\:7b7e\:7cfb\:7edf\:7248\:672c"], tagSystem["\:6807\:7b7e"]][[1]]


NestedRuleToList[r_] := r /. Rule -> List //Flatten


ShortenTagName[tagName_List] := With[{flatTN = tagName},
	Delete[flatTN, #] & /@ Subsets@Position[flatTN, s_String /; StringTake[s, -1] === "@", {1}]
]


ShortenTagName[tagName_Rule] := ShortenTagName@NestedRuleToList@tagName


GenerateShortenDictionary[tagNames_] :=
	NestedRuleToList/@tagNames //AssociationMap[ShortenTagName] //KeyValueMap[OperatorApplied[Rule][#]/@#2&] //Catenate//Association


QueryByHeadSubsequence[{q__String}, dict_Association] :=Position[Keys@dict, {q, ___}] //Replace[{
	{} :> Missing["NotFound"],
	{single_} :> Extract[dict, single],
	multiple:_List :> Extract[dict, multiple]
}]


getRestByCommonLast[long_, common_] :=
	Replace[res:Except@_Missing :> Reverse@Take[#, First@res-1]]@FirstPosition[Last@common]@# &@Reverse@long
(*Reap@NestWhile[(Sow@#1;#2)&@@TakeDrop[#,-1]&, long, #=!={}&&UnsameQ@@Last/@{#,common}&,1,10] //
	MapAt[Reverse@Catenate@#[[1]]&, {2}]*)


QueryTagFullName[query_List, dict_] := Catch[With[{queries = Drop[query,-#]&/@(Range@Length@query-1)},
	Function[q, Switch[#,
		_Missing, #,
		{{__String}..}, Throw[{#, getRestByCommonLast[query, #]}&@Fold[LongestCommonSubsequence, #], QueryTagFullName],
		{__String}, Throw[{#, getRestByCommonLast[query, #]}, QueryTagFullName]
	] &@QueryByHeadSubsequence[q, dict]] /@ queries
], QueryTagFullName]


End[]


EndPackage[]
