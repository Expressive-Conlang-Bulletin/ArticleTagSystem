#!/usr/bin/env wolframscript
(* ::Package:: *)

If[$Notebooks, SetDirectory@NotebookDirectory[]]


<<CATSER.wl


src = Import["../tags/CATSER.yml.m"];


If[$Notebooks, ResetDirectory[]]


tns = GetTagNames@src


dict = GenerateShortenDictionary[tns]


Normal[dict][[15;;30]]


dict[NestedRuleToList["0.0.0"->"\:8bed\:4e49"->"\:6982\:5ff5"->"\:601d\:7ef4"->"\:4fee\:9970"]]


QueryTagFullName[NestedRuleToList["0.0.0"->"\:8bed\:4e49"->"\:6982\:5ff5"->"\:601d\:7ef4"->"\:4fee\:9970"->"1"->"2"],dict]


QueryTagFullName[NestedRuleToList["0.0.0"->"\:8bed\:4e49"->"\:6982\:5ff5"->"\:601d\:7ef4"->"1"->"2"],dict]


QueryTagFullName[{"1"},dict]
QueryTagFullName[{"0.0.0"},dict]


dict@NestedRuleToList["0.0.0"->"\:8bed\:4e49"->"\:6982\:5ff5"->"\:601d\:7ef4"->"\:4fee\:9970"]
