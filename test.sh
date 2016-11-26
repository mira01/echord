rm *.beam
erlc *.erl
erl -noshell -eval 'eunit:test(zpevnik), init:stop().'
