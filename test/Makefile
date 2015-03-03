.PHONY: all simple_html simple_md simple_xml clean

all: simple_html simple_md simple_xml

simple_html:
	OCAMLRUNPARAM=b ../tower build simple_html -o simple_html_result
	diff simple_html_correct simple_html_result

simple_md:
	OCAMLRUNPARAM=b ../tower build simple_md -o simple_md_result
	diff simple_md_correct simple_md_result

simple_xml:
	OCAMLRUNPARAM=b ../tower build simple_xml -o simple_xml_result
	diff simple_xml_correct simple_xml_result

clean:
	rm -rf simple_html_result/*
	rm -rf simple_md_result/*
	rm -rf simple_xml_result/*
