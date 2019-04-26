package com.github.bohnman.squiggly.function;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * A result object returned from a function match execution.
 */
public class FunctionMatchResult {
    private Object input;
    @Nullable
    private SquigglyFunction<Object> winner;
    private Score score = Score.EMPTY;
    private final List<Object> parameters;

    /**
     * Construct.
     *
     * @param request the request that this result was initiated from
     */
    public FunctionMatchResult(FunctionMatchRequest request) {
        this.input = request.getInput();
        this.parameters = new ArrayList<>(request.getParameters());
    }

    /**
     * Get the input object.
     *
     * @return input
     */
    public Object getInput() {
        return input;
    }

    public void setInput(Object input) {
        this.input = input;
    }

    /**
     * Get the winner or null if no winner.
     *
     * @return winner
     */
    @Nullable
    public SquigglyFunction<Object> getWinner() {
        return winner;
    }

    public void setWinner(SquigglyFunction<Object> winner) {
        this.winner = winner;
    }

    /**
     * Get the parameters to apply.
     *
     * @return params
     */
    public List<Object> getParameters() {
        return parameters;
    }

    /**
     * Get the score of the match.
     *
     * @return score
     */
    public Score getScore() {
        return score;
    }

    public void setScore(Score score) {
        this.score = score;
    }

    /**
     * A score helps pick the best match.
     */
    public static class Score implements Comparable<Score> {

        /**
         * Indicate no score.
         */
        public static final Score EMPTY = new Score();

        private int assignable;
        private int assignableDistance;
        private int convertible;
        private int convertibleDistance;
        private int undefined;
        private int undefinedDistance;

        /**
         * Indicate an exact match.
         *
         * @return score
         */
        public Score exact() {
            return assignable(0);
        }

        /**
         * Indicate a match where the source type was assignable to the target type.
         *
         * @param distance hierarchy distance between the source and target type
         * @return score
         */
        public Score assignable(int distance) {
            this.assignable++;
            this.assignableDistance += distance;
            return this;
        }

        /**
         * Indicate a match where the source type could be converted to the target type.
         *
         * @param distance number of conversions taken to get to the target type
         * @return score.
         */
        public Score convertible(int distance) {
            this.convertible++;
            this.convertibleDistance += distance;
            return this;
        }

        /**
         * Indicate a match where the source is null.
         *
         * @param distance hierchary distance between the target type and Object
         * @return score
         */
        public Score undefined(int distance) {
            this.undefined++;
            this.undefinedDistance += distance;
            return this;
        }

        /**
         * Returns true if there are no assignable, convertible, or undefined matches.
         *
         * @return empty
         */
        public boolean isEmpty() {
            return (assignable + convertible + undefined) == 0;
        }

        /**
         * Returns true if there is a least 1 assignable, convertible, or undefined match.
         *
         * @return not empty
         */
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
