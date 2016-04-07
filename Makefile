run-cpp:
	sbt "run TestRunner --backend c --compile --test --genHarness"

run-cpp-vcd:
	sbt "run TestRunner --backend c --compile --test --genHarness --vcd"

run-vcs:
	sbt "run TestRunner --backend v --genHarness"

clean:
	rm -f *Module*
	rm -f *.cpp
	rm -f *.o
	rm -f *.h
	rm -f *.v
	rm -f vpi*
	rm -f *.vpd
	rm -f *.vcd
	rm -f opendatabase.log
	rm -rf DVEfiles
