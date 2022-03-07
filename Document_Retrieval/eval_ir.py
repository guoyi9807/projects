"""\
--------------------------------------------------------------------------------
    USE: python <PROGNAME> (options) keyfile response
    ACTION: computes IR system performance measures, given input files:
        * 'keyfile' - a "gold standard" indicating the documents that 
                      are relevant to each query, and 
        * 'response' - the documents retrieved for each query by the system.
    MAJOR OPTIONS:
        -h : print this help message
        -n INT : only consider the first INT responses for each query
    SPECIAL OPTIONS:
        -q : print scores for each individual question (not just global averages)
        -f : print summary scores in \"flat\" mode (i.e. as numbers on single line)
        -F : print terse flat summary - shows only P, R, F scores (on single line)
    DATAFORMAT:
        In both input files, each line specifies two alpha-numeric strings, in 
        the manner:
         QID  DOCID
        which are identifiers for a query (QID) and a document (DOCID), respectively.
        In the 'response' file, the order of lines is assumed to give the "ranking" 
        of documents returned for each query by the system, with the 'best' (rank 1) 
        document being listed first, and so on. This rank order is used with the 
        "-n" option, in selecting the responses that are retained.
--------------------------------------------------------------------------------
"""

import sys, re
import getopt

class CommandLine:
    def __init__(self):
        opts, args = getopt.getopt(sys.argv[1:],'hn:qfF')
        opts = dict(opts)

        if '-h' in opts:
            self.print_help()

        if len(args) == 2:
            self.keyfile = args[0]
            self.responsefile = args[1]
        else:
            print('\n*** ERROR: must specify precisely 2 arg files (key,response) ***', file=sys.stderr)
            self.print_help()
            
        if '-n' in opts:
            self.response_limit = int(opts['-n'])
        else:
            self.response_limit = None

        self.query_print = '-q' in opts

        self.print_flat = '-f' in opts

        self.print_terse_flat = '-F' in opts

    def print_help(self):
        progname = sys.argv[0]
        progname = progname.split('/')[-1] # strip off extended path
        help = __doc__.replace('<PROGNAME>', progname, 1)
        print(help, file=sys.stderr)
        sys.exit()

class Key:
    def __init__(self,config):
        skip = re.compile('^\s*($|#)')
        key = open(config.keyfile,'r')
        self.relevant = {}
        for line in key:
            if skip.search(line):
                continue
            vals = line.split()
            if len(vals) == 2:
                qid, docid = vals
                if qid not in self.relevant:
                    self.relevant[qid] = set()
                self.relevant[qid].add(docid)
            else:
                msg = 'ERROR: bad line in key file:<%s>' % line
                raise Exception(msg)
        key.close()
    
    def is_relevant(self,qid,docid):
        if qid in self.relevant:
            if docid in self.relevant[qid]:
                return True
        return False

    def num_relevant(self,qid):
        if qid in self.relevant:
            return len(self.relevant[qid])
        return 0
    
    def qids(self):
        return set(self.relevant.keys())
     
class Response:
    def __init__(self,config,key):
        seen = {}
        self.retrieved = {}
        self.rel_ranks = {}
        skip = re.compile('^\s*($|#)')
        response = open(config.responsefile,'r')
        for line in response:
            if skip.search(line): continue
            vals = line.split()
            if len(vals) != 2:
                msg = 'ERROR: bad line in key file:<%s>' % line
                raise Exception(msg)
            qid, docid = vals
            if qid not in seen:
                seen[qid] = set()
                self.retrieved[qid] = 0
                self.rel_ranks[qid] = []
            if (config.response_limit and self.retrieved[qid] >= config.response_limit):
                # response limit specified and reached, so this response ignored 
                continue
            self.retrieved[qid] += 1
            if key.is_relevant(qid,docid) and docid not in seen[qid]:
                self.rel_ranks[qid].append(self.retrieved[qid])
            # duplicate entries are counted, but only *credited* at first occurrence. 
            seen[qid].add(docid)            
        response.close()

    def get_ranks(self,qid):
        if qid in self.rel_ranks:
            return self.rel_ranks[qid]
        return []
    
    def qids(self):
        return set(self.retrieved.keys())

    def num_retrieved(self,qid):
        if qid in self.retrieved:
            return self.retrieved[qid]
        return 0
    
    def num_relevant_retrieved(self,qid):
        if qid in self.rel_ranks:
            return len(self.rel_ranks[qid])
        return 0

class Score:
    def __init__(self,config,key,response):
        self.all_queries = sorted(key.qids() | response.qids(), key=lambda Id:int(Id[1:]))
        self.num_queries = len(self.all_queries)
        self.total_relevant = 0
        self.total_retrieved = 0
        self.total_relevant_retrieved = 0
        
        for qid in self.all_queries:
            
            rel = key.num_relevant(qid)
            ret = response.num_retrieved(qid)
            rel_ret = response.num_relevant_retrieved(qid)
            self.total_relevant  += rel
            self.total_retrieved += ret
            self.total_relevant_retrieved += rel_ret

            if config.query_print:
                self.print_measure1_query(qid,ret,rel,rel_ret)

    def print_measure1_query(self,qid,ret,rel,rel_ret):
        print(("Query ID: %s\n"
               "Total number of documents\n"
               "    Retrieved:       %4d\n"
               "    Relevant:        %4d\n"
               "    Rel_Retr:        %4d\n"
        ) % (qid,ret,rel,rel_ret), file=sys.stdout, end='')
    
    def print_measure1_summary(self,config):
        if self.total_retrieved > 0:
            precision = float(self.total_relevant_retrieved)/self.total_retrieved
        else: 
            precision = 0
        if self.total_relevant > 0: 
            recall = float(self.total_relevant_retrieved)/self.total_relevant
        else:
            recall = 0
        if precision + recall > 0:
            fmeasure = (2 * precision * recall)/(precision + recall)
        else:
            fmeasure = 0.0
        if config.print_terse_flat:
            format = "N:{3} P:{4:.2f} R:{5:.2f} F:{6:.2f}"
        elif config.print_flat:
            format = "{0} {1} {2} {3} {4:.2f} {5:.2f} {6:.2f}"
        else:
            format = (
            "-------------------------------------------\n"
            "Total number of queries: {0}\n"
            "Total number of documents over all queries:\n"
            "    Retrieved:       {1:>4}\n"
            "    Relevant:        {2:>4}\n"
            "    Rel_Retr:        {3:>4}\n"
            "Prec/Rec/F across all queries:\n"
            "    Precision:       {4:.2f}\n"
            "    Recall:          {5:.2f}\n"
            "    F-measure:       {6:.2f}\n"
            )
        scores = (
            self.num_queries,
            self.total_retrieved,
            self.total_relevant,
            self.total_relevant_retrieved,
            precision, recall, fmeasure)
        print(format.format(*scores), file=sys.stdout, end='')

if __name__ == '__main__':
    config = CommandLine()
    key = Key(config)
    response = Response(config,key)
    scorer = Score(config,key,response)
    scorer.print_measure1_summary(config)
