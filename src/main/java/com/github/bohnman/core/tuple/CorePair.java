package com.github.bohnman.core.tuple;

import javax.annotation.Nullable;
import java.util.Objects;

public class CorePair<L, R> {

    @Nullable
    private final L left;

    @Nullable
    private final R right;

    private CorePair(@Nullable L left, @Nullable R right) {
        this.left = left;
        this.right = right;
    }

    @Nullable
    public L getLeft() {
        return left;
    }

    @Nullable
    public R getRight() {
        return right;
    }

    public static <L, R> CorePair<L, R> of(@Nullable L left, @Nullable  R right) {
        return new CorePair<>(left, right);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CorePair<?, ?> pair = (CorePair<?, ?>) o;
        return Objects.equals(left, pair.left) &&
                Objects.equals(right, pair.right);
    }

    @Override
    public int hashCode() {
        return Objects.hash(left, right);
    }

    @Override
    public String toString() {
        return "Pair{" +
                "left=" + left +
                ", right=" + right +
                '}';
    }
}
