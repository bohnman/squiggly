package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.squiggly.function.SquigglyFunctionClass;

/**
 * Class that defines the default functions to include.
 */
@SquigglyFunctionClass(include = {
        CollectionFunctions.class,
        CoreJsonNodeFunctions.class,
        DateFunctions.class,
        NumberFunctions.class,
        MixedFunctions.class,
        ObjectFunctions.class,
        StringFunctions.class
})
public class DefaultFunctions {

    private DefaultFunctions() {
    }

}
