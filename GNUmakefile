# GNU 'make' file for greenery

SHELL	= bash
PY2	= python
PY3	= python3
PYTHONS	= $(PY2) $(PY3) 
PYTEST	= -m pytest -v --capture=no

all: help

help:
	@echo "Manage testing and installation across all supported/available Pythons"
	@echo
	@echo "help     This help"
	@echo "test     Test greenery on all supported/available Pythons"
	@echo "clean    Remove generated files"
	@echo "install  Install on all supported/available Pythons"
	@echo "upload   Upload new version to python.org (must log in)"

# Test on all supported/available Pythons, using py.test or direct testing
TESTS	= greenery/lego_test.py						\
	  greenery/fsm_test.py						\
	  greenery/v1_test.py
test: clean
	@for py in $(PYTHONS); do					\
	    if ! $$py -V 2>/dev/null; then				\
		echo "$$py not available; not testing";			\
		continue;						\
	    fi;								\
	    if $$py -c 'import pytest' >/dev/null 2>&1; then		\
		if ! $$py $(PYTEST); then				\
		    (( failed++ ));					\
		fi;							\
	    else							\
		for t in $(TESTS); do 					\
		    echo -n $$t;					\
		    if $$py $$t; then					\
			echo " PASSED";					\
		    else						\
			echo " FAILED";					\
			(( failed++ ));					\
		    fi;							\
		done;							\
	    fi;								\
	done; exit $${failed:-0}

clean:
	@rm -rf dist build *.egg-info */__pycache__ *.pyc */*.pyc

install:
	@for py in $(PYTHONS); do					\
	    if ! $$py -V 2>/dev/null; then				\
	        echo "$$py not available; not installing";		\
	    else							\
		if ! make clean || ! $$py setup.py install; then	\
		    echo "$$py installation failed";			\
		fi;							\
	    fi;								\
	done

upload:
	python setup.py sdist upload
