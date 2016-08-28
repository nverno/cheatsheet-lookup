

if __name__ == '__main__':
    import json
    with open("sources.json", "r") as f:
        config = json.load(f)

    for src in config:
        elem = config[src]
        uri = elem['source']
        try:
            lang = elem['indexer']['language']
            prog = elem['indexer']['program']
            opts = elem['indexer']['options']
            print "Getting index from %s: %s %s %s" % (uri, lang, prog, ' '.join(opts))
        except:
            "Failed to parse %s" % uri
