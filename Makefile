

all : compile;

% : sbt_%;

sbt_%:
	./sbt --warn $*

format : scalafmt;

clean:
	rm -rf target

# clean, and every cache, so the next build starts from nothing and
# downloads everything again:
# - here: sbt's build of the build (project/), the IDE's (.bloop, .metals,
#   .bsp), wrangler's (.wrangler) and the tests' scratch (test_work);
# - this user's, shared with every other build on this machine: sbt 2's
#   disk cache, sbt's launcher (~/.sbt/boot), coursier's downloads (the JVM
#   and sbt that ./sbt runs, and every dependency), and npx's (wrangler).
# Not the workspace (the grader's state, mirror and outbox), nor what's
# published locally (~/.ivy2/local) or sbt's settings (~/.sbt/1.0).
CACHE_HOME := $(or $(XDG_CACHE_HOME),$(HOME)/.cache)
COURSIER := $(or $(COURSIER_CACHE),$(CACHE_HOME)/coursier)

purge : clean
	rm -rf project/target project/project/target
	rm -rf .bloop .metals .bsp .wrangler test_work
	test -n "$(HOME)" && rm -rf "$(CACHE_HOME)/sbt" "$(COURSIER)" \
	  "$(HOME)/.sbt/boot" "$(HOME)/.npm/_npx"

