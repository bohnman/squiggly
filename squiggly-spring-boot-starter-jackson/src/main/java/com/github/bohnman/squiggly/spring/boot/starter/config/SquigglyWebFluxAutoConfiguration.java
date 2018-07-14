package com.github.bohnman.squiggly.spring.boot.starter.config;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SequenceWriter;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.context.provider.ThreadLocalSquigglyContextProvider;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.core.holder.SquigglyFilterHolder;
import com.github.bohnman.squiggly.core.variable.SquigglyVariablesHolder;
import com.github.bohnman.squiggly.jackson.Squiggly;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.ResolvableType;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.codec.json.Jackson2JsonEncoder;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.util.MimeType;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.reactive.config.WebFluxConfigurationSupport;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

/**
 * Configuration registered when the WebFlux api is in the project.
 */
@SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
@Configuration
@ConditionalOnWebApplication
@ConditionalOnClass(DispatcherHandler.class)
@AutoConfigureAfter(JacksonAutoConfiguration.class)
public class SquigglyWebFluxAutoConfiguration {

    private static final String FILTER_HINT = SquigglyWebFluxAutoConfiguration.class.getName() + ".filter";
    private static final String VARIABLES_HINT = SquigglyWebFluxAutoConfiguration.class.getName() + ".variables";

    @Autowired(required = false)
    SquigglyFilterCustomizer filterCustomizer;

    /**
     * Register a provier that looks for a filter in the request.
     *
     * @return context provider.
     */
    @Bean
    @ConditionalOnMissingBean
    public SquigglyContextProvider squigglyRequestContextProvider() {
        return new ThreadLocalSquigglyContextProvider() {
            @Nullable
            @Override
            protected String customizeFilter(@Nullable String filter, @Nullable Class beanClass) {
                return (filterCustomizer == null) ? filter : filterCustomizer.apply(filter, beanClass);
            }
        };
    }

    /**
     * Register an application listener.
     *
     * @param squiggly squiggly object
     * @return listener
     */
    @Bean
    public static SquigglyAutoConfiguration.SquigglyApplicationListener squigglyApplicationListener(Squiggly squiggly) {
        return new RequestSquigglyApplicationListener(squiggly);
    }

    /**
     * Application listener.
     */
    public static class RequestSquigglyApplicationListener extends SquigglyAutoConfiguration.SquigglyApplicationListener {
        public RequestSquigglyApplicationListener(Squiggly squiggly) {
            super(squiggly);
        }
    }


    /**
     * Registers a json encoder that can use squiggly.
     */
    @SuppressWarnings({"NullableProblems", "SpringJavaInjectionPointsAutowiringInspection"})
    @Configuration
    public static class WebFluxConfig extends WebFluxConfigurationSupport {

        @Autowired
        private ObjectMapper objectMapper;

        @Autowired
        private SquigglyConfig config;


        @Override
        @Bean
        public ServerCodecConfigurer serverCodecConfigurer() {
            ServerCodecConfigurer configurer = super.serverCodecConfigurer();
            configurer.defaultCodecs().jackson2JsonEncoder(new JsonEncoder(objectMapper, config));
            return configurer;
        }
    }

    @SuppressWarnings("unchecked")
    private static class JsonEncoder extends Jackson2JsonEncoder {

        private final SquigglyConfig config;

        public JsonEncoder(ObjectMapper mapper, SquigglyConfig config) {
            super(mapper);
            this.config = config;
        }

        @Override
        public Map<String, Object> getEncodeHints(ResolvableType actualType, ResolvableType elementType, MediaType mediaType, ServerHttpRequest request, ServerHttpResponse response) {
            Map<String, String> queryParams = request.getQueryParams().toSingleValueMap();

            Map<String, Object> hints = new HashMap<>(super.getEncodeHints(actualType, elementType, mediaType, request, response));
            hints.put(FILTER_HINT, queryParams.get(config.getFilterRequestParam()));
            hints.put(VARIABLES_HINT, queryParams);
            return hints;
        }

        @Override
        protected ObjectWriter customizeWriter(ObjectWriter writer, MimeType mimeType, ResolvableType elementType, Map<String, Object> hints) {
            String filter = (String) hints.get(FILTER_HINT);

            if (filter == null) {
                return writer;
            }


            return new ObjectWriterWrapper(writer, filter, (Map<String, Object>) hints.get(VARIABLES_HINT));
        }
    }

    private static class ObjectWriterWrapper extends ObjectWriter {

        private final String filter;
        private final Map<String, Object> variables;

        public ObjectWriterWrapper(ObjectWriter base, String filter, Map<String, Object> variables) {
            super(base, base.getConfig());
            this.filter = filter;
            this.variables = variables;
        }

        @Override
        public void writeValue(JsonGenerator gen, Object value) throws IOException {
            try {
                setThreadLocals();
                super.writeValue(gen, value);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public ObjectWriter withView(Class<?> view) {
            try {
                setThreadLocals();
                return super.withView(view);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValues(File out) throws IOException {
            try {
                setThreadLocals();
                return super.writeValues(out);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValues(JsonGenerator gen) throws IOException {
            try {
                setThreadLocals();
                return super.writeValues(gen);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValues(Writer out) throws IOException {
            try {
                setThreadLocals();
                return super.writeValues(out);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValues(OutputStream out) throws IOException {
            try {
                setThreadLocals();
                return super.writeValues(out);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(File out) throws IOException {
            try {
                setThreadLocals();
                return super.writeValuesAsArray(out);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(JsonGenerator gen) throws IOException {
            try {
                setThreadLocals();
                return super.writeValuesAsArray(gen);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(Writer out) throws IOException {
            try {
                setThreadLocals();
                return super.writeValuesAsArray(out);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(OutputStream out) throws IOException {
            try {
                setThreadLocals();
                return super.writeValuesAsArray(out);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public void writeValue(File resultFile, Object value) throws IOException {
            try {
                setThreadLocals();
                super.writeValue(resultFile, value);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public void writeValue(OutputStream out, Object value) throws IOException {
            try {
                setThreadLocals();
                super.writeValue(out, value);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public void writeValue(Writer w, Object value) throws IOException {
            try {
                setThreadLocals();
                super.writeValue(w, value);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public String writeValueAsString(Object value) throws JsonProcessingException {
            try {
                setThreadLocals();
                return super.writeValueAsString(value);
            } finally {
                removeThreadLocals();
            }
        }

        @Override
        public byte[] writeValueAsBytes(Object value) throws JsonProcessingException {
            try {
                setThreadLocals();
                return super.writeValueAsBytes(value);
            } finally {
                removeThreadLocals();
            }
        }

        private void setThreadLocals() {
            SquigglyFilterHolder.set(filter);
            SquigglyVariablesHolder.set(variables);
        }

        private void removeThreadLocals() {
            SquigglyFilterHolder.remove();
            SquigglyVariablesHolder.remove();
        }

    }
}
