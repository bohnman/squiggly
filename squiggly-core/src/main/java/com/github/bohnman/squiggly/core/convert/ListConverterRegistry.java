package com.github.bohnman.squiggly.core.convert;

import javax.annotation.concurrent.NotThreadSafe;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

@NotThreadSafe
public class ListConverterRegistry implements SquigglyConverterRegistry {

    private final List<ConverterRecord> records = new ArrayList<>();

    @Override
    public List<ConverterRecord> findAll() {
        return Collections.unmodifiableList(records);
    }

    @Override
    public void addAll(Collection<ConverterRecord> records) {
        this.records.addAll(records);
    }

    @Override
    public void add(ConverterRecord record) {
        records.add(record);
    }

    @Override
    public <S, T> void add(Class<S> source, Class<T> target, Function<S, T> converter) {
        add(new ConverterRecord(source, target, converter, records.size()));
    }
}
