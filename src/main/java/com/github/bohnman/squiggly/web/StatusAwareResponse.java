package com.github.bohnman.squiggly.web;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import java.io.IOException;

public class StatusAwareResponse extends HttpServletResponseWrapper {

    private int status = HttpServletResponse.SC_OK;

    public StatusAwareResponse(HttpServletResponse response) {
        super(response);
    }

    public int getStatus() {
        return status;
    }

    @Override
    public void sendError(int sc, String msg) throws IOException {
        this.status = sc;
        super.sendError(sc, msg);
    }

    @Override
    public void sendError(int sc) throws IOException {
        this.status = sc;
        super.sendError(sc);
    }

    @Override
    public void setStatus(int sc) {
        this.status = sc;
        super.setStatus(sc);
    }

    @Override
    public void setStatus(int sc, String sm) {
        this.status = sc;
        super.setStatus(sc, sm);
    }
}
