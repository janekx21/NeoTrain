#!/bin/bash

python convert.py | elm-format --stdin --yes --output "../src/Generated/Layouts.elm"

