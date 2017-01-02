ERL = erl -boot start_clean

.PHONY: all test clean

all:
	mkdir -p ebin/ ;
	cp -f src/topicaclplugin_src.app ebin/topicaclplugin.app ;
	erl -make ;

test: all
	mkdir -p logs/ ;
	ct_run -pa ebin/ -spec topicaclplugin.spec ;

clean:
	rm -Rf logs/ ebin/ ;

