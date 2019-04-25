package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.squiggly.core.function.annotations.SquigglyFunctionClass;

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
