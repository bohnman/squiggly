package com.github.bohnman.squiggly.path;

import javax.annotation.Nullable;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;

import static java.util.Objects.requireNonNull;

public class SquigglyPaths {

    public static SquigglyObjectPath objectPath(String name, Class<?> type) {
        return new InternalObjectPath(0, name, type);
    }

    private static class InternalObjectPath implements SquigglyObjectPath {

        @Nullable
        private InternalObjectPath child;

        private final int depth;

        private final String name;

        @Nullable
        private InternalObjectPath parent;

        @Nullable
        private InternalObjectPath root;

        private final Class<?> type;

        private InternalObjectPath(int depth, String name, Class<?> type) {
            this.depth = depth;
            this.name = requireNonNull(name);
            this.type = requireNonNull(type);
        }

        @Override
        public SquigglyObjectPath append(String name, Class<?> type) {
            InternalObjectPath newParent = new InternalObjectPath(depth, name, type);
            newParent.parent = parent;
            newParent.root = (root == this) ? newParent : root;

            InternalObjectPath child = new InternalObjectPath(depth + 1, name, type);
            newParent.child = this;
            child.parent = newParent;
            child.root = newParent.root;

            return child;
        }

        @Override
        public String getName() {
            return name;
        }

        @Nullable
        @Override
        public SquigglyObjectPath getParent() {
            return parent;
        }

        @Nullable
        @Override
        public SquigglyObjectPath getRoot() {
            return root;
        }

        @Override
        public Class<?> getType() {
            return type;
        }

        @Override
        public Iterator<SquigglyObjectPath> iterator() {
            return new Iterator<SquigglyObjectPath>() {
                private InternalObjectPath next = root;

                @Override
                public boolean hasNext() {
                    return next != null && next.child != null;
                }

                @Override
                public SquigglyObjectPath next() {
                    InternalObjectPath child = next.child;

                    if (child == null) {
                        throw new NoSuchElementException();
                    }

                    next = child;
                    return child;
                }
            };
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            InternalObjectPath that = (InternalObjectPath) o;

            if (this.depth != that.depth) {
                return false;
            }

            return ancestorEquals(that);
        }

        private boolean ancestorEquals(InternalObjectPath that) {
            if (!name.equals(that.name)) {
                return false;
            }

            if (!type.equals(that.type)) {
                return false;
            }

            if (parent == null && that.parent == null) {
                return true;
            }

            if (parent == null || that.parent == null) {
                return false;
            }

            return parent.ancestorEquals(that.parent);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, parent, type);
        }

        public static InternalObjectPath create(String name, Class<?> type) {
            InternalObjectPath path = new InternalObjectPath(0, name, type);
            path.root = path;

            return path;
        }
    }
}
