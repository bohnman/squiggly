package com.github.bohnman.squiggly;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.FilterProvider;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.github.bohnman.squiggly.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilter;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilterMixin;
import com.github.bohnman.squiggly.parser.SquigglyParser;

/**
 * Provides various way of registering a {@link SquigglyPropertyFilter} with a Jackson ObjectMapper.
 */
public class Squiggly {

    private Squiggly() {
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a static filter expression.
     *
     * @param mapper the Jackson Object Mapper
     * @param filter the filter expressions
     * @return object mapper, mainly for convenience
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static ObjectMapper init(ObjectMapper mapper, String filter) throws IllegalStateException {
        return init(mapper, new SimpleSquigglyContextProvider(new SquigglyParser(), filter));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a static filter expression.
     *
     * @param mappers the Jackson Object Mappers to init
     * @param filter the filter expressions
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, String filter) throws IllegalStateException {
        init(mappers, new SimpleSquigglyContextProvider(new SquigglyParser(), filter));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific context provider.
     *
     * @param mapper          the Jackson Object Mapper
     * @param contextProvider the context provider to use
     * @return object mapper, mainly for convenience
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static ObjectMapper init(ObjectMapper mapper, SquigglyContextProvider contextProvider) throws IllegalStateException {
        return init(mapper, new SquigglyPropertyFilter(contextProvider));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific context provider.
     *
     * @param mappers          the Jackson Object Mappers to init
     * @param contextProvider the context provider to use
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, SquigglyContextProvider contextProvider) {
        init(mappers, new SquigglyPropertyFilter(contextProvider));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific property filter.
     *
     * @param mapper the Jackson Object Mapper
     * @param filter the property filter
     * @return object mapper, mainly for convenience
     * @throws IllegalStateException if the filter was unable to be registered
     */
    @SuppressWarnings("deprecation")
    public static ObjectMapper init(ObjectMapper mapper, SquigglyPropertyFilter filter) throws IllegalStateException {
        FilterProvider filterProvider = mapper.getSerializationConfig().getFilterProvider();
        SimpleFilterProvider simpleFilterProvider;

        if (filterProvider instanceof SimpleFilterProvider) {
            simpleFilterProvider = (SimpleFilterProvider) filterProvider;
        } else if (filterProvider == null) {
            simpleFilterProvider = new SimpleFilterProvider();
            mapper.setFilters(simpleFilterProvider);
        } else {
            throw new IllegalStateException("Unable to register squiggly filter with FilterProvider of type " + filterProvider.getClass().getName() + ".  You'll have to register the filter manually");

        }

        simpleFilterProvider.addFilter(SquigglyPropertyFilter.FILTER_ID, filter);
        mapper.addMixIn(Object.class, SquigglyPropertyFilterMixin.class);

        return mapper;
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific property filter.
     *
     * @param mappers the Jackson Object Mappers to init
     * @param filter the property filter
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, SquigglyPropertyFilter filter) {
        for (ObjectMapper mapper : mappers) {
            init(mapper, filter);
        }
    }

}
