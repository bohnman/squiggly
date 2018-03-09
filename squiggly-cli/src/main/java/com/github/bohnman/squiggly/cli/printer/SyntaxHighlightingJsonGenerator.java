package com.github.bohnman.squiggly.cli.printer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.SerializableString;
import com.fasterxml.jackson.core.util.JsonGeneratorDelegate;

import java.io.IOException;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;

public class SyntaxHighlightingJsonGenerator extends JsonGeneratorDelegate {

    private final SyntaxHighlighter syntaxHighlighter;

    public SyntaxHighlightingJsonGenerator(JsonGenerator d, SyntaxHighlighter syntaxHighlighter) {
        super(d, false);
        this.syntaxHighlighter = syntaxHighlighter;
    }


    @Override
    public void writeBoolean(boolean state) throws IOException {
        writeRaw(syntaxHighlighter.getBooleanColor());
        super.writeBoolean(state);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeFieldName(String name) throws IOException {
        writeRaw(syntaxHighlighter.getFieldNameColor());
        super.writeFieldName(name);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeFieldName(SerializableString name) throws IOException {
        writeRaw(syntaxHighlighter.getFieldNameColor());
        super.writeFieldName(name);
        writeRaw(syntaxHighlighter.getReset());
    }

    public void writeFieldId(long id) throws IOException {
        writeRaw(syntaxHighlighter.getFieldNameColor());
        super.writeFieldName(Long.toString(id));
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(short v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNull() throws IOException {
        writeRaw(syntaxHighlighter.getNullColor());
        super.writeNull();
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(int v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(long v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(BigInteger v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(double v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(float v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(BigDecimal v) throws IOException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(v);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeNumber(String encodedValue) throws IOException, UnsupportedOperationException {
        writeRaw(syntaxHighlighter.getNumberColor());
        super.writeNumber(encodedValue);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeString(String text) throws IOException {
        writeRaw(syntaxHighlighter.getStringColor());
        super.writeString(text);
        writeRaw(syntaxHighlighter.getReset());
    }

    public void writeString(Reader reader, int len) {
        throw new UnsupportedOperationException("writeString(Reader,int)");
    }

    @Override
    public void writeString(char[] text, int offset, int len) throws IOException {
        writeRaw(syntaxHighlighter.getStringColor());
        super.writeString(text, offset, len);
        writeRaw(syntaxHighlighter.getReset());
    }

    @Override
    public void writeString(SerializableString text) throws IOException {
        writeRaw(syntaxHighlighter.getStringColor());
        super.writeString(text);
        writeRaw(syntaxHighlighter.getReset());
    }
}
