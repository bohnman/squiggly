package dev.nicklasw.squiggly.name;

public interface SquigglyName {

    String getName();

    String getRawName();

    int match(String name);
}
