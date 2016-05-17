config-value-getopt
===================

The config-value-getopt package allows GetOpt[1] specified options
to be loaded from a configuration file in the config-value[2] format.

* config-value section names are matched against the "long" option names
in GetOpt.
* Argument values can be provided as strings or numbers
* An option will be omitted if its value is set to `no`
* An option's argument will be omitted if its value is set to `yes`

Example:

```
address:       "::"
port:          9000
no-access-log: yes
hostname:      no
```

translates to

```
--address="::" --port="9000" --no-access-log
```

1. https://hackage.haskell.org/package/base-4.8.2.0/docs/System-Console-GetOpt.html
2. https://hackage.haskell.org/package/config-value
