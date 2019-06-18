package com.github.bohnman.squiggly.execute;

import com.github.bohnman.squiggly.node.FilterNode;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;

import static java.util.Objects.requireNonNull;

public class SquigglyExecutions {

    private SquigglyExecutions() {
    }

    public static SquigglyExecutionInterceptor compositeExecuionInterceptor(SquigglyExecutionInterceptor... interceptors) {
        return new CompositeExecutionInterceptor(Arrays.asList(interceptors));
    }

    public static SquigglyExecutionInterceptor compositeExecutionInterceptor(List<SquigglyExecutionInterceptor> interceptors) {
        return new CompositeExecutionInterceptor(interceptors);
    }

    public static SquigglyExecutionInterceptor toExecutionInterceptor(SquigglyFilterInterceptor interceptor) {
        return new SquigglyExecutionInterceptor() {
            @Nullable
            @Override
            public String applyFilter(SquigglyPathContext context, String filter) {
                return interceptor.applyFilter(context, filter);
            }
        };
    }

    public static SquigglyExecutionInterceptor toExecutionInterceptor(SquigglyFilterNodeInterceptor interceptor) {
        return new SquigglyExecutionInterceptor() {
            @Nullable
            @Override
            public FilterNode applyFilterNode(SquigglyFilterContext context, FilterNode node) {
                return interceptor.applyFilterNode(context, node);
            }
        };
    }

    public static SquigglyExecutionInterceptor modifyFilterWhenType(Class<?> type, String prefix, String suffix) {
        requireNonNull(type);
        requireNonNull(prefix);
        requireNonNull(suffix);

        return new SquigglyExecutionInterceptor() {
            @Nullable
            @Override
            public String applyFilter(SquigglyPathContext context, String filter) {
                if (type.isAssignableFrom(context.getType())) {
                    return prefix + filter + suffix;
                }

                return filter;
            }
        };
    }

    private static class CompositeExecutionInterceptor implements SquigglyExecutionInterceptor {

        private final List<SquigglyExecutionInterceptor> interceptors;

        private CompositeExecutionInterceptor(List<SquigglyExecutionInterceptor> interceptors) {
            requireNonNull(interceptors);
            this.interceptors = interceptors;
        }

        @Nullable
        @Override
        public String applyFilter(SquigglyPathContext context, String filter) {
            for (SquigglyExecutionInterceptor interceptor : interceptors) {
                filter = interceptor.applyFilter(context, filter);

                if (filter == null) {
                    return null;
                }
            }

            return filter;
        }

        @Nullable
        @Override
        public FilterNode applyFilterNode(SquigglyFilterContext context, FilterNode node) {
            for (SquigglyExecutionInterceptor interceptor : interceptors) {
                node = interceptor.applyFilterNode(context, node);

                if (node == null) {
                    return null;
                }
            }

            return node;
        }
    }
}
