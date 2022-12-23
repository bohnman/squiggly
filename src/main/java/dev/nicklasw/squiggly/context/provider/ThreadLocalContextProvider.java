package dev.nicklasw.squiggly.context.provider;

public class ThreadLocalContextProvider extends AbstractSquigglyContextProvider {

    @Override
    protected String getFilter(Class beanClass) {
        return SquigglyFilterHolder.getFilter();
    }
}
