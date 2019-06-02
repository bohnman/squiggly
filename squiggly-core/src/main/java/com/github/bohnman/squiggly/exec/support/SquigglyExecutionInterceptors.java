package com.github.bohnman.squiggly.exec.support;

import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.squiggly.exec.SquigglyExecution;
import com.github.bohnman.squiggly.exec.SquigglyExecutionInterceptor;
import com.github.bohnman.squiggly.node.support.FilterNode;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static java.util.Objects.requireNonNull;

public class SquigglyExecutionInterceptors {

    private SquigglyExecutionInterceptors() {
    }

    public static SquigglyExecutionInterceptor modifyWhenType(Class<?> type, String prefix, String suffix) {
        requireNonNull(type);
        requireNonNull(prefix);
        requireNonNull(suffix);

        return execution -> {
            if (type.isAssignableFrom(execution.getType())) {
                return execution.withFilter(prefix + execution.getFilter() + suffix);
            }

            return execution;
        };
    }

    public static SquigglyExecutionInterceptor modifyFilter(Function<SquigglyExecution, String> interceptor) {
        requireNonNull(interceptor);

        return execution -> {
            String filter = interceptor.apply(execution);

            if (filter == null) {
                return execution.cancel();
            }

            if (!filter.equals(execution.getFilter())) {
                return execution.withFilter(filter);
            }

            return execution;
        };
    }

    public static SquigglyExecutionInterceptor modifyFilterNode(Function<SquigglyExecution, FilterNode> interceptor) {
        requireNonNull(interceptor);

        return execution -> {
            FilterNode node = interceptor.apply(execution);

            if (node == null) {
                return execution.cancel();
            }

            if (!node.equals(execution.getFilterNode())) {
                return execution.withFilterNode(node);
            }

            return execution;
        };
    }

    public static SquigglyExecutionInterceptor of(SquigglyExecutionInterceptor... interceptors) {
        return new CompositeExecutionInterceptor(Collections.unmodifiableList(Arrays.asList(interceptors)));
    }

    public static SquigglyExecutionInterceptor of(Iterable<SquigglyExecutionInterceptor> interceptors) {
        return new CompositeExecutionInterceptor(Collections.unmodifiableList(CoreLists.create(interceptors)));
    }

    private static class CompositeExecutionInterceptor implements SquigglyExecutionInterceptor {

        private final List<SquigglyExecutionInterceptor> interceptors;

        private CompositeExecutionInterceptor(List<SquigglyExecutionInterceptor> interceptors) {
            requireNonNull(interceptors);
            this.interceptors = interceptors;
        }

        @Override
        public SquigglyExecution apply(SquigglyExecution execution) {
            for (SquigglyExecutionInterceptor interceptor : interceptors) {
                execution = interceptor.apply(execution);

                if (execution.isCancelled()) {
                    break;
                }
            }

            return execution;
        }
    }
}
