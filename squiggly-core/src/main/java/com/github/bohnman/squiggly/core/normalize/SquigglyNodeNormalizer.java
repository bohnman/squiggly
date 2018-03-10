package com.github.bohnman.squiggly.core.normalize;

import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.parser.SquigglyNode;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SquigglyNodeNormalizer {

    private final BaseSquiggly squiggly;

    public SquigglyNodeNormalizer(BaseSquiggly squiggly) {
        this.squiggly = CoreAssert.notNull(squiggly);
    }

    public SquigglyNode normalize(SquigglyNode node) {
        if (node.isVariable()) {
            SquigglyVariableResolver variableResolver = squiggly.getVariableResolver();
            String value = Objects.toString(variableResolver.resolveVariable(node.getName()));

            if (value == null) {
                value = '$' + node.getName();
            }

            node = node.withName(new ExactName(value));
        }

        List<SquigglyNode> newChildren = null;

        for (int i = 0; i < node.getChildren().size(); i++) {
            SquigglyNode child = node.getChildren().get(i);
            SquigglyNode normalizedChild = normalize(child);

            if (child != normalizedChild) {
                if (newChildren == null) newChildren = new ArrayList<>(node.getChildren());
                newChildren.set(i, normalizedChild);
            }
        }

        if (newChildren != null) {
            node = node.withChildren(newChildren);
        }

        return node;
    }
}
