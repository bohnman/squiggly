package com.github.bohnman.squiggly.exec;

@FunctionalInterface
public interface SquigglyExecutionInterceptor {

    SquigglyExecution apply(SquigglyExecution execution);
}
