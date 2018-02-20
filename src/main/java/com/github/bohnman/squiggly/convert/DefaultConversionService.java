package com.github.bohnman.squiggly.convert;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

public class DefaultConversionService implements SquigglyConversionService {

    private static ConverterRecord NO_MATCH = new ConverterRecord(Object.class, Object.class, Function.identity());

    private final LoadingCache<Key, ConverterRecord> cache;

    public DefaultConversionService(SquigglyConfig config, List<ConverterRecord> records) {
        this.cache = CacheBuilder.from(config.getConvertCacheSpec())
                .build(new CacheLoader<Key, ConverterRecord>() {
                    @Override
                    public ConverterRecord load(@Nonnull Key key) {
                        return DefaultConversionService.this.load(key);
                    }
                });

        records.forEach(record -> {
            Key key = Key.from(record);
            cache.put(key, record);
            load(key);
        });
    }

    private ConverterRecord load(Key key) {
        Class<?> source = key.source;

        if (source == Object.class) {
            return NO_MATCH;
        }

        Class<?> superSource = source.getSuperclass();

        ConverterRecord record = load(new Key(superSource, key.target));

        if (record == NO_MATCH || superSource == Object.class) {
            for (Class<?> ifaceClass : source.getInterfaces()) {
                ConverterRecord ifaceRecord = load(new Key(ifaceClass, key.target));

                if (ifaceRecord != NO_MATCH && (record == NO_MATCH || ifaceRecord.getSource() != Object.class)) {
                    record = ifaceRecord;
                    break;
                }
            }
        }

        return record;
    }

    @Override
    public boolean canConvert(Class<?> source, Class<?> target) {
        if (source == target) {
            return true;
        }

        ConverterRecord record = cache.getUnchecked(new Key(source, target));

        if (record == NO_MATCH) {
            return false;
        }

        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T convert(Object value, Class<T> target) {
        if (value == null) {
            return null;
        }

        if (value.getClass() == target) {
            return (T) value;
        }

        ConverterRecord record = cache.getUnchecked(new Key(value.getClass(), target));

        if (record == NO_MATCH) {
            throw new IllegalArgumentException(String.format("Cannot convert [%s] from [%s] to [%s]: No converter found", value, value.getClass(), value.getClass()));
        }

        Function converter = record.getConverter();
        return (T) converter.apply(value);
    }

    private static class Key {
        private final Class<?> source;
        private final Class<?> target;

        public Key(Class<?> source, Class<?> target) {
            this.source = source;
            this.target = target;
        }

        public static Key from(ConverterRecord record) {
            return new Key(record.getSource(), record.getTarget());
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Key key = (Key) o;
            return Objects.equals(source, key.source) &&
                    Objects.equals(target, key.target);
        }

        @Override
        public int hashCode() {
            return Objects.hash(source, target);
        }
    }
}
