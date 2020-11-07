# csv2influx

This is a tool to convert csv files into [Influx Line
Protocol](influxline).  I use it when I export some data from influxdb
I need to manipulate and send back.

## Synopsis

```
Usage: csv2influx [-m|--measurement ARG] [-t|--tags ARG] [-f|--fields ARG]
                  [--time ARG] FILES...
  Convert CSV to influxdb wire protocol

Available options:
  -m,--measurement ARG     measurement column name (default: "measurement")
  -t,--tags ARG            tags column names (comma separated)
  -f,--fields ARG          field column names (comma separated)
  --time ARG               timestamp column name (default: "time")
  -h,--help                Show this help text
```

## Example:

If you have a csv file named `env.csv` that looks like this:

```csv
name,time,sensor,site,temperature
env,1599612130709842134,freezer,site1,-10.82
env,1599612130743049304,freezer,site1,-10.82
env,1599612306307120495,freezer,site1,-10.82
```

and you want to store the into InfluxDB under the measurement named by
the field `name` with tags named by `sensor` and `site`, a field value
named by `temperature` and the timestamp named by `time`, you'd run
the following command:

```sh
csv2influx -m name -t sensor,site -f temperature --time time env.csv > env.lines
```

and it'll spit out the following:

```
env,site=site1,sensor=freezer temperature=-10.82 1599612130709842134
env,site=site1,sensor=freezer temperature=-10.82 1599612130743049304
env,site=site1,sensor=freezer temperature=-10.82 1599612306307120495
```

You can feed that into influxdb with a simple `curl` command:

```
curl -i -XPOST 'http://$myinfluxdb:8086/write?db=$dbname' --data-binary @env.lines
```

[influxline]: https://docs.influxdata.com/influxdb/v2.0/reference/syntax/line-protocol/
