TESTS  := simple
RunDir := RunDir

DEFAULT: help

help:
	@echo "====================== targets ============================"
	@echo "         run: run the tests (use TESTS= to select a subset)"
	@echo "        prep: prepare tests to be run"
	@echo "       clean: delete the RunDir"
	@echo "    demolish: delete all temporary files (may be dangerous)"
	@echo "====================== targets ============================"

run: prep
	python ../utils/lit/lit.py -s $(RunDir)
  
prep:
	rm -rf $(RunDir)
	mkdir  $(RunDir)
	cp -R $(TESTS) $(RunDir)

.PHONEY: clean demolish
clean: 
	rm -rf $(RunDir)

demolish: 
	find . -name '*.grin_*'       | xargs rm
	find . -name '*.grin2_*'      | xargs rm
	find . -name '*.grin2'        | xargs rm
	find . -name '*.hcr'          | xargs rm
	find . -name '*.hi'           | xargs rm
	find . -name '*.o'            | xargs rm
	find . -name '*.c'            | xargs rm
	find . -name 'Output' -type d | xargs rm -rf
	find . -name '*.dSYM' -type d | xargs rm -rf

