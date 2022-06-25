#!/usr/bin/env wolframscript
(* ::Package:: *)

If[$Notebooks, SetDirectory@NotebookDirectory[]]


src = Import["../tags/CATSER.yml.m"];


tns = GetTagNames@src


GenerateShortenDictionary@tns


dict = GenerateShortenDictionary[tns]


Normal[dict][[15;;30]]


dict[NestedRuleToList["0.0.0"->"\:8bed\:4e49"->"\:6982\:5ff5"->"\:601d\:7ef4"->"\:4fee\:9970"]]


If[$Notebooks, ResetDirectory[]]
