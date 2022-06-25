(* ::Package:: *)

BeginPackage["CATSER`"]


Names@"`*"


Quiet[Remove@"`*", {Remove::rmnsm}]


GetTagNames
ShortenTagName
QueryTagFullName
NestedRuleToList
GenerateShortenDictionary


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


QueryTagFullName[q_, dict_] := NestWhile[, {dict[q]}, ]


End[]


EndPackage[]
