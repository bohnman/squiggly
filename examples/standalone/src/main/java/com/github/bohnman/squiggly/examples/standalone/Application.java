package com.github.bohnman.squiggly.examples.standalone;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.examples.standalone.model.Issue;

import java.io.File;

public class Application {

    public static void main(String[] args) throws Exception {
        Object model = null;
        String filter = null;

        if (args.length == 0) {
            model = Issue.findAll();
            filter = "**";
        } else if (args.length == 1) {
            model = Issue.findAll();
            filter = args[0];
        } else if (args.length == 2) {
            model = new ObjectMapper().readValue(new File(args[0]), Object.class);
            filter = args[1];
        } else {
            printUsage();
            System.exit(1);
        }

        ObjectMapper objectMapper = Squiggly.init(new ObjectMapper(), filter);
        objectMapper.writeValue(System.out, model);
    }

    private static void printUsage() {
        String prefix = "mvn compile exec:java";
        String usage = "Usage:\n" +
                prefix + '\n' +
                prefix + " " + "-Dexec.args='filter'\n" +
                prefix + " " + "-Dexec.args='json-file filter'\n";

        System.out.println(usage);
    }
}
