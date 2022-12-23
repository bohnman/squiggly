package dev.nicklasw.squiggly.examples.servlet.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import dev.nicklasw.squiggly.Squiggly;
import dev.nicklasw.squiggly.examples.servlet.web.ListResponse;
import dev.nicklasw.squiggly.web.RequestSquigglyContextProvider;

import jakarta.servlet.http.HttpServletRequest;

public class Jackson {

    private static final ObjectMapper OBJECT_MAPPER = Squiggly.init(new ObjectMapper(), new RequestSquigglyContextProvider() {
        @Override
        protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {

            // OPTIONAL: automatically wrap filter expressions in items{} when the object is a ListResponse
            if (filter != null && ListResponse.class.isAssignableFrom(beanClass)) {
                filter = "items{" + filter + "}";
            }

            return filter;
        }
    });

    public static ObjectMapper objectMapper() {
        return OBJECT_MAPPER;
    }
}
