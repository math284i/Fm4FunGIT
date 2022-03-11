#!/bin/bash
mono FsLexYacc.10.0.0/build/fslex/net46/fslex.exe Fm4FunLexer.fsl --unicode
mono FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe Fm4FunParser.fsp --module Fm4FunParser
fsharpi Fm4Fun.fsx

