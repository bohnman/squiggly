package com.github.bohnman.core.jsr305;

import javax.annotation.Nonnull;
import javax.annotation.meta.TypeQualifierDefault;
import javax.annotation.meta.When;
import java.lang.annotation.ElementType;

@Nonnull(when = When.MAYBE)
@TypeQualifierDefault({
        ElementType.FIELD,
        ElementType.METHOD,
        ElementType.PARAMETER,
        ElementType.TYPE_USE})
public @interface NullableByDefault {
}
