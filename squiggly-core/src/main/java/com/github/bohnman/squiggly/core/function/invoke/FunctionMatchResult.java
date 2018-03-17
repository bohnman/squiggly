package com.github.bohnman.squiggly.core.function.invoke;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;

import java.util.ArrayList;
import java.util.List;

public class FunctionMatchResult {
    private Object input;
    private SquigglyFunction<Object> winner;
    private Score score = Score.EMPTY;
    private final List<Object> parameters;

    public FunctionMatchResult(FunctionMatchRequest request) {
        this.input = request.getInput();
        this.parameters = new ArrayList<>(request.getParameters());
    }

    public Object getInput() {
        return input;
    }

    public void setInput(Object input) {
        this.input = input;
    }

    public SquigglyFunction<Object> getWinner() {
        return winner;
    }

    public void setWinner(SquigglyFunction<Object> winner) {
        this.winner = winner;
    }

    public List<Object> getParameters() {
        return parameters;
    }

    public Score getScore() {
        return score;
    }

    public void setScore(Score score) {
        this.score = score;
    }

    public static class Score implements Comparable<Score> {
        public static final Score EMPTY = new Score();

        private int assignable;
        private int assignableDistance;
        private int convertible;
        private int convertibleDistance;
        private int undefined;
        private int undefinedDistance;

        public Score exact() {
            return assignable(0);
        }

        public Score assignable(int distance) {
            this.assignable++;
            this.assignableDistance += distance;
            return this;
        }

        public Score convertible(int distance) {
            this.convertible++;
            this.convertibleDistance += distance;
            return this;
        }

        public Score undefined(int distance) {
            this.undefined++;
            this.undefinedDistance += distance;
            return this;
        }

        public boolean isEmpty() {
            return (assignable + convertible + undefined) == 0;
        }

        public boolean isNotEmpty() {
            return !isEmpty();
        }

        @Override
        public int compareTo(Score o) {
            int cmp = Integer.compare(assignable, o.assignable);
            if (cmp == 0) cmp = -1 * Integer.compare(assignableDistance, o.assignableDistance);
            if (cmp == 0) cmp = Integer.compare(convertible, o.convertible);
            if (cmp == 0) cmp = -1 * Integer.compare(convertibleDistance, o.convertibleDistance);
            if (cmp == 0) cmp = Integer.compare(undefined, o.undefined);
            if (cmp == 0) cmp = -1 * Integer.compare(undefinedDistance, o.undefinedDistance);
            return cmp;
        }
    }
}
