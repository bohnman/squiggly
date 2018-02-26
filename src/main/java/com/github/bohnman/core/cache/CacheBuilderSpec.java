/*
 * Copyright (C) 2011 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.github.bohnman.core.cache;


import com.github.bohnman.core.lang.CoreStrings;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static com.github.bohnman.core.lang.CoreAssert.isTrue;
import static java.util.stream.Collectors.toList;

/**
 * A specification of a {@link CacheBuilder} configuration.
 * <p>
 * <p>{@code CacheBuilderSpec} supports parsing configuration off of a string, which makes it
 * especially useful for command-line configuration of a {@code CacheBuilder}.
 * <p>
 * <p>The string syntax is a series of comma-separated keys or key-value pairs, each corresponding
 * to a {@code CacheBuilder} method.
 * <ul>
 * <li>{@code maximumSize=[long]}: sets {@link CacheBuilder#setMaximumWeight(long)}.
 * <li>{@code maximumWeight=[long]}: sets {@link CacheBuilder#maximumWeight}.
 * <li>{@code expireAfterAccess=[duration]}: sets {@link CacheBuilder#setExpireAfterAccess(long, TimeUnit)}.
 * <li>{@code expireAfterWrite=[duration]}: sets {@link CacheBuilder#setExpireAfterWrite(long, TimeUnit)}.
 * </ul>
 * <p>
 * <p>The set of supported keys will grow as {@code CacheBuilder} evolves, but existing keys will
 * never be removed.
 * <p>
 * <p>Durations are represented by an integer, followed by one of "d", "h", "m", or "s",
 * representing days, hours, minutes, or seconds respectively. (There is currently no syntax to
 * request expiration in milliseconds, microseconds, or nanoseconds.)
 * <p>
 * <p>Whitespace before and after commas and equal signs is ignored. Keys may not be repeated; it is
 * also illegal to use the following pairs of keys in a single value:
 * <ul>
 * <li>{@code maximumSize} and {@code maximumWeight}
 * <li>{@code softValues} and {@code weakValues}
 * </ul>
 * <p>
 * <p>{@code CacheBuilderSpec} does not support configuring {@code CacheBuilder} methods with
 * non-value parameters. These must be configured in code.
 * <p>
 * <p>A new {@code CacheBuilder} can be instantiated from a {@code CacheBuilderSpec} using
 * {@link CacheBuilder#from(CacheBuilderSpec)} or {@link CacheBuilder#from(String)}.
 *
 * @author Adam Winer
 * @since 12.0
 */
public final class CacheBuilderSpec {
    /**
     * Parses a single value.
     */
    private interface ValueParser {
        void parse(CacheBuilderSpec spec, String key, @Nullable String value);
    }

    /**
     * Map of names to ValueParser.
     */
    private static final Map<String, ValueParser> VALUE_PARSERS;


    static {
        Map<String, ValueParser> valueParserMap = new HashMap<>();
        valueParserMap.put("maximumSize", new MaximumSizeParser());
        valueParserMap.put("maximumWeight", new MaximumWeightParser());
        valueParserMap.put("expireAfterAccess", new AccessDurationParser());
        valueParserMap.put("expireAfterWrite", new WriteDurationParser());
        VALUE_PARSERS = Collections.unmodifiableMap(valueParserMap);
    }

    private Long maximumSize;
    private Long maximumWeight;
    private long writeExpirationDuration;
    private TimeUnit writeExpirationTimeUnit;
    private long accessExpirationDuration;
    private TimeUnit accessExpirationTimeUnit;

    /**
     * Specification; used for toParseableString().
     */
    private final String specification;

    private CacheBuilderSpec(String specification) {
        this.specification = specification;
    }

    /**
     * Creates a CacheBuilderSpec from a string.
     *
     * @param cacheBuilderSpecification the string form
     */
    public static CacheBuilderSpec parse(String cacheBuilderSpecification) {

        CacheBuilderSpec spec = new CacheBuilderSpec(cacheBuilderSpecification);
        if (!cacheBuilderSpecification.isEmpty()) {
            List<String> keyValuePairs = Arrays.stream(CoreStrings.split(cacheBuilderSpecification, ","))
                    .map(String::trim)
                    .collect(toList());

            for (String keyValuePair : keyValuePairs) {
                List<String> keyAndValue = Arrays.stream(CoreStrings.split(keyValuePair, "="))
                        .map(String::trim)
                        .collect(toList());


                isTrue(!keyAndValue.isEmpty(), "blank key-value pair");
                isTrue(
                        keyAndValue.size() <= 2,
                        String.format("key-value pair %s with more than one equals sign",
                        keyValuePair));

                // Find the ValueParser for the current key.
                String key = keyAndValue.get(0);
                ValueParser valueParser = VALUE_PARSERS.get(key);
                isTrue(valueParser != null, String.format("unknown key %s", key));

                String value = keyAndValue.size() == 1 ? null : keyAndValue.get(1);
                valueParser.parse(spec, key, value);
            }
        }

        return spec;
    }

    /**
     * Returns a CacheBuilderSpec that will prevent caching.
     */
    public static CacheBuilderSpec disableCaching() {
        // Maximum size of zero is one way to block caching
        return CacheBuilderSpec.parse("maximumSize=0");
    }

    /**
     * Returns a CacheBuilder configured according to this instance's specification.
     */
    CacheBuilder<Object, Object> toCacheBuilder() {
        CacheBuilder<Object, Object> builder = CacheBuilder.builder();

        if (maximumSize != null) {
            builder.setMaximumWeight(maximumSize);
            builder.setWeigher((o, o2) -> 1);
        }
        if (maximumWeight != null) {
            builder.setMaximumWeight(maximumWeight);
        }

        if (writeExpirationTimeUnit != null) {
            builder.setExpireAfterWrite(writeExpirationDuration, writeExpirationTimeUnit);
        }
        if (accessExpirationTimeUnit != null) {
            builder.setExpireAfterAccess(accessExpirationDuration, accessExpirationTimeUnit);
        }

        return builder;
    }

    /**
     * Returns a string that can be used to parse an equivalent {@code CacheBuilderSpec}. The order
     * and form of this representation is not guaranteed, except that reparsing its output will
     * produce a {@code CacheBuilderSpec} equal to this instance.
     */
    public String toParsableString() {
        return specification;
    }

    @Override
    public String toString() {
        return "CacheBuilderSpec{" +
                "maximumSize=" + maximumSize +
                ", maximumWeight=" + maximumWeight +
                ", writeExpirationDuration=" + writeExpirationDuration +
                ", writeExpirationTimeUnit=" + writeExpirationTimeUnit +
                ", accessExpirationDuration=" + accessExpirationDuration +
                ", accessExpirationTimeUnit=" + accessExpirationTimeUnit +
                ", specification='" + specification + '\'' +
                ", parsableString='" + toParsableString() + '\'' +
                '}';
    }

    /**
     * Returns a string representation for this CacheBuilderSpec instance. The form of this
     * representation is not guaranteed.
     */


    @Override
    public int hashCode() {
        return Objects.hash(
                maximumSize,
                maximumWeight,
                durationInNanos(writeExpirationDuration, writeExpirationTimeUnit),
                durationInNanos(accessExpirationDuration, accessExpirationTimeUnit));
    }

    @Override
    public boolean equals(@Nullable Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof CacheBuilderSpec)) {
            return false;
        }
        CacheBuilderSpec that = (CacheBuilderSpec) obj;
        return Objects.equals(maximumSize, that.maximumSize)
                && Objects.equals(maximumWeight, that.maximumWeight)
                && Objects.equals(
                durationInNanos(writeExpirationDuration, writeExpirationTimeUnit),
                durationInNanos(that.writeExpirationDuration, that.writeExpirationTimeUnit))
                && Objects.equals(
                durationInNanos(accessExpirationDuration, accessExpirationTimeUnit),
                durationInNanos(that.accessExpirationDuration, that.accessExpirationTimeUnit));
    }

    /**
     * Converts an expiration duration/unit pair into a single Long for hashing and equality. Uses
     * nanos to match CacheBuilder implementation.
     */
    @Nullable
    private static Long durationInNanos(long duration, @Nullable TimeUnit unit) {
        return (unit == null) ? null : unit.toNanos(duration);
    }

    /**
     * Base class for parsing integers.
     */
    abstract static class IntegerParser implements ValueParser {
        protected abstract void parseInteger(CacheBuilderSpec spec, int value);

        @Override
        public void parse(CacheBuilderSpec spec, String key, String value) {
            isTrue(value != null && !value.isEmpty(), String.format("value of key %s omitted", key));
            try {
                parseInteger(spec, Integer.parseInt(value));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(
                        format("key %s value set to %s, must be integer", key, value), e);
            }
        }
    }

    /**
     * Base class for parsing integers.
     */
    abstract static class LongParser implements ValueParser {
        protected abstract void parseLong(CacheBuilderSpec spec, long value);

        @Override
        public void parse(CacheBuilderSpec spec, String key, String value) {
            isTrue(value != null && !value.isEmpty(), String.format("value of key %s omitted", key));
            try {
                parseLong(spec, Long.parseLong(value));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(
                        format("key %s value set to %s, must be integer", key, value), e);
            }
        }
    }

    /**
     * Parse maximumSize
     */
    static class MaximumSizeParser extends LongParser {
        @Override
        protected void parseLong(CacheBuilderSpec spec, long value) {
            isTrue(spec.maximumSize == null, String.format("maximum size was already set to %s", spec.maximumSize));
            isTrue(
                    spec.maximumWeight == null, String.format("maximum weight was already set to %s", spec.maximumWeight));
            spec.maximumSize = value;
        }
    }

    /**
     * Parse maximumWeight
     */
    static class MaximumWeightParser extends LongParser {
        @Override
        protected void parseLong(CacheBuilderSpec spec, long value) {
            isTrue(
                    spec.maximumWeight == null, String.format("maximum weight was already set to %s", spec.maximumWeight));
            isTrue(spec.maximumSize == null, String.format("maximum size was already set to %s", spec.maximumSize));
            spec.maximumWeight = value;
        }
    }


    /**
     * Base class for parsing times with durations
     */
    abstract static class DurationParser implements ValueParser {
        protected abstract void parseDuration(CacheBuilderSpec spec, long duration, TimeUnit unit);

        @Override
        public void parse(CacheBuilderSpec spec, String key, String value) {
            isTrue(value != null && !value.isEmpty(), String.format("value of key %s omitted", key));
            try {
                char lastChar = value.charAt(value.length() - 1);
                TimeUnit timeUnit;
                switch (lastChar) {
                    case 'd':
                        timeUnit = TimeUnit.DAYS;
                        break;
                    case 'h':
                        timeUnit = TimeUnit.HOURS;
                        break;
                    case 'm':
                        timeUnit = TimeUnit.MINUTES;
                        break;
                    case 's':
                        timeUnit = TimeUnit.SECONDS;
                        break;
                    default:
                        throw new IllegalArgumentException(
                                format(
                                        "key %s invalid format.  was %s, must end with one of [dDhHmMsS]", key, value));
                }

                long duration = Long.parseLong(value.substring(0, value.length() - 1));
                parseDuration(spec, duration, timeUnit);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(
                        format("key %s value set to %s, must be integer", key, value));
            }
        }
    }

    /**
     * Parse expireAfterAccess
     */
    static class AccessDurationParser extends DurationParser {
        @Override
        protected void parseDuration(CacheBuilderSpec spec, long duration, TimeUnit unit) {
            isTrue(spec.accessExpirationTimeUnit == null, "expireAfterAccess already set");
            spec.accessExpirationDuration = duration;
            spec.accessExpirationTimeUnit = unit;
        }
    }

    /**
     * Parse expireAfterWrite
     */
    static class WriteDurationParser extends DurationParser {
        @Override
        protected void parseDuration(CacheBuilderSpec spec, long duration, TimeUnit unit) {
            isTrue(spec.writeExpirationTimeUnit == null, "expireAfterWrite already set");
            spec.writeExpirationDuration = duration;
            spec.writeExpirationTimeUnit = unit;
        }
    }

    private static String format(String format, Object... args) {
        return String.format(Locale.ROOT, format, args);
    }
}
