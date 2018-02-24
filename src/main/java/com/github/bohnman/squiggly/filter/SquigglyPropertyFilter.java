package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.filter.function.FunctionInvoker;
import com.github.bohnman.squiggly.filter.match.SquigglyNodeMatcher;
import com.github.bohnman.squiggly.parser.SquigglyNode;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import net.jcip.annotations.ThreadSafe;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.bohnman.squiggly.filter.match.SquigglyNodeMatcher.NEVER_MATCH;
import static com.google.common.base.Preconditions.checkNotNull;


/**
 * A Jackson @{@link com.fasterxml.jackson.databind.ser.PropertyFilter} that filters objects using squiggly syntax.
 * <p>Here are some examples of squiggly syntax:</p>
 * <pre>
 *    // grab the id and name fields
 *    id,name
 *
 *    // grab the id and nested first name and last name from a the user property
 *    id,user{firstName,lastName}
 *
 *    // grab the full object graph
 *    **
 *
 *    // grab just the base fields
 *    base
 *
 *    // grab all fields of the current object, but just the base fields of nested objects
 *    *
 *
 *    // grab fields starting with eco
 *    eco*
 *
 *    // grab fields ending with Time
 *    *Time
 *
 *    // grab fields containing Weight
 *    *Weight*
 *
 *    // grab the firstName field of the nested employee and manager objects
 *    employee{firstName},manager{firstName}
 *    employee|manager{firstName}
 *
 *    // grab all fields annotated with @PropertyView("hardware") or a derived annotation
 *    hardware
 * </pre>
 */
@ThreadSafe
public class SquigglyPropertyFilter extends SimpleBeanPropertyFilter {

    public static final String FILTER_ID = "squigglyFilter";

    private final Squiggly squiggly;
    private final FunctionInvoker functionInvoker;
    private final SquigglyNodeMatcher nodeMatcher;

    /**
     * Constructor.
     *
     * @param squiggly squiggly
     */
    public SquigglyPropertyFilter(Squiggly squiggly) {
        this.squiggly = checkNotNull(squiggly);
        this.nodeMatcher = new SquigglyNodeMatcher(squiggly);
        this.functionInvoker = new FunctionInvoker(squiggly.getConversionService(), squiggly.getFunctionRepository(), squiggly.getVariableResolver());
    }

    @Override
    protected boolean include(final BeanPropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @Override
    protected boolean include(final PropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @Override
    public void serializeAsField(final Object pojo, final JsonGenerator jgen, final SerializerProvider provider,
                                 final PropertyWriter writer) throws Exception {
        SquigglyNode match = nodeMatcher.match(writer, jgen);

        if (match != null && match != NEVER_MATCH) {
            if ((match.getKeyFunctions().isEmpty() && match.getValueFunctions().isEmpty()) || !(writer instanceof BeanPropertyWriter)) {
                squiggly.getSerializer().serializeAsIncludedField(pojo, jgen, provider, writer);
            } else {
                BeanPropertyWriter beanPropertyWriter = (BeanPropertyWriter) writer;
                String name = "" + functionInvoker.invoke(writer.getName(), match.getKeyFunctions());
                Object value = functionInvoker.invoke(beanPropertyWriter.get(pojo), match.getValueFunctions());
                squiggly.getSerializer().serializeAsConvertedField(pojo, jgen, provider, writer, name, value);
            }
        } else if (!jgen.canOmitFields()) {
            squiggly.getSerializer().serializeAsExcludedField(pojo, jgen, provider, writer);
        }
    }

    public static void main(String[] args) {
        ObjectMapper mapper = Squiggly.init(new ObjectMapper(), "nickNames.map(@name->@name['name'])");
        System.out.println(SquigglyUtils.stringify(mapper, new Person("Ryan", "Bohn", "rbohn", "bohnman", "doogie")));
    }

    public static class NickName {
        private final String name;

        public NickName(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    private static class Person {
        private final String firstName;
        private final String lastName;
        private List<NickName> nickNames;

        public Person(String firstName, String lastName, String... nickNames) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.nickNames = Arrays.stream(nickNames).map(NickName::new).collect(Collectors.toList());
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public List<NickName> getNickNames() {
            return nickNames;
        }
    }
}
