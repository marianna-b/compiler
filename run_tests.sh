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
			echo "failed, see $test.ll.out"
		else
			echo "passed"
			rm "$test".ll.out
		fi
	else
		echo "failed"
	fi
done
