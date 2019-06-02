package com.github.bohnman.squiggly.convert.support;

import com.github.bohnman.squiggly.convert.ConverterDescriptor;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;

import javax.annotation.concurrent.NotThreadSafe;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * A converter registry backed by a list.
 */
@NotThreadSafe
public class ListConverterRegistry implements SquigglyConverterRegistry {

    private final List<ConverterDescriptor> records = new ArrayList<>();

    @Override
    public List<ConverterDescriptor> findAll() {
        return Collections.unmodifiableList(records);
    }

    @Override
    public void addAll(Collection<ConverterDescriptor> records) {
        this.records.addAll(records);
    }

    @Override
    public void add(ConverterDescriptor record) {
        records.add(record);
    }

    @Override
    public <S, T> void add(Class<S> source, Class<T> target, Function<S, T> converter) {
        add(new ConverterDescriptor(source, target, converter, records.size()));
    }
}
