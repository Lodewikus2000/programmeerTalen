
JFLAGS = -g
JAVAC  = javac
JAVA   = java

JAVAS   = $(shell find ./ -type f -name '*.java')
CLASSES = $(patsubst %.java,%.class,$(JAVAS))
DIAGRAM_SCRIPTS  = $(shell find ./individual/ -type f -name 'generate_diagram_*.sh')
DIAGRAM_DOTS = $(shell find ./individual -type f -name '*.dot')
DAT_FILES = $(shell find hc-logs/ sa-logs/ -type f -name '*.dat')
PLOTS = $(shell find -type f -name 'hc-*.pdf'; find -type f -name 'sa-*.pdf')

all: 	$(CLASSES)


%.class: %.java
	$(JAVAC) $(FLAGS) $<

TESTS_JAVAS = $(shell find ./tests/ -type f -name 'Test_*.java')
TESTS_CLASSES = $(patsubst %.java,%.class,$(TESTS_JAVAS))

test:	$(CLASSES)
	@for test in $(TESTS_CLASSES) ; do \
	  echo "##### running test: $$test" ;\
          dir=`dirname $$test` ;\
	  base=`basename $$test` ;\
	  file=`echo $${base%.*}` ;\
	  $(JAVA) -classpath ./:$$dir $$file ;\
	done

doc:
	mkdir -p doc
	javadoc -d doc $(JAVAS)

doc_clean:
	rm -rf doc

diagram_dot:
	@for diagram in $(DIAGRAMS_SCRIPTS) ; do \
	  echo "##### making dot: $$diagram" ;\
      dir=`dirname $$individual` ;\
	  base=`basename $$diagram` ;\
	  file=`echo $${base%.*}` ;\
  	  `./:$$dir $$file` ;\
	done

diagrams:
	@for diagram in $(DIAGRAM_DOTS) ; do\
	  echo "##### making dot: $$diagram" ;\
	  dir=`dirname $$individual` ;\
	  base=`basename $$diagram` ;\
	  file=`echo $${base%.*}` ;\
	  `./:$$dir $$file` ;\
	  `dot -Tpdf :$$dir $$file > :$$dir $$file .pdf`;\

hc:
	./team/hc_experiments.sh;
	./team/hc_statistics.sh;
	./team/hc_plots.sh;

sa:
	./team/sa_experiments.sh;
	./team/sa_statistics.sh;
	./team/sa_plots.sh;

clean:  doc_clean
	$(RM) $(CLASSES) $(DIAGRAM_DOTS) $(DAT_FILES) $(PLOTS)
