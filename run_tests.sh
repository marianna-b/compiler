#!/usr/bin/env bash

rm test/*.ll
rm test/*.out
for test in ./test/*.in ; do
	test=${test%%.in}
	echo -n "Testing $test... "
	stack exec compiler "$test".in > "$test".ll
	if [[ -f "$test".ll.a ]]; then
		diff "$test".ll "$test".ll.a > "$test".ll.out
		if [[ $? != 0 ]]; then
			echo -n "codegen failed, see $test.ll.out "
		else
			echo -n "codegen passed "
			rm "$test".ll.out
		fi
	else
		echo -n "codegen failed - not found "
	fi
	stack exec compiler ast "$test".in > "$test".ast
	if [[ -f "$test".ast.a ]]; then
		diff "$test".ast "$test".ast.a > "$test".ast.out
		if [[ $? != 0 ]]; then
			echo "ast failed, see $test.ast.out"
		else
			echo "ast passed"
			rm "$test".ast.out
		fi
	else
		echo "ast failed - not found"
	fi
done
