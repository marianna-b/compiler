#!/usr/bin/env bash

./run_tests.sh
for test in ./test/*.in; do
	test=${test%%.in}
	cp "$test".ll "$test".ll.a
	cp "$test".ast "$test".ast.a
done
