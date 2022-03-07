import math
class Retrieve:
    
    # Create new Retrieve object storing index and term weighting 
    # scheme. (You can extend this method, as required.)
    def __init__(self, index, term_weighting):
        self.index = index
        self.term_weighting = term_weighting
        self.num_docs = self.compute_number_of_documents()
        # precompute size of documents which contains terms
        self.doc_size = dict()
        if self.term_weighting == 'binary':
            for doc_dict in self.index.values():
                for docid in doc_dict.keys():
                    self.doc_size[docid] = self.doc_size.get(docid, 0) + 1
        elif self.term_weighting == 'tf':
            for doc_dict in self.index.values():
                for docid, tfrq in doc_dict.items():
                    self.doc_size[docid] = self.doc_size.get(docid, 0) + tfrq * tfrq
        elif self.term_weighting == 'tfidf':
            # precompute idf
            self.idf_dict = dict()
            for term, doc_dict in self.index.items():
                self.idf_dict[term] = math.log10(self.num_docs / len(doc_dict))
            for term, doc_dict in self.index.items():
                idf = self.idf_dict[term]
                for docid, tfrq in doc_dict.items():
                    self.doc_size[docid] = self.doc_size.get(docid, 0) + tfrq * idf * tfrq * idf
        for docid, doc_size_square in self.doc_size.items():
            self.doc_size[docid] = math.sqrt(doc_size_square) # this will return a dict with docid: document size

    # define a binary model to calculate the cosine similarity
    def binary(self, query):
        cos_values = dict()
        term_doc = set()
        term_doc = self.index.keys()
        query_all_term = set()
        query_all_term = query
        ids = self.doc_ids
        # match term in query and index
        for docid in ids:
            q_dot_d = 0
            for term in query_all_term:
                if term in term_doc and docid in self.index[term].keys():
                    query_i = 1
                    doc_i = 1
                    # calculation of numerator
                    q_dot_d += query_i * doc_i
            cosine = q_dot_d / self.doc_size[docid]
            # recording cosine similarity values per docid
            cos_values[docid] = cosine
        # finished processing all the documents and returning top 10 ranked results
        final_ten = sorted(cos_values, key = lambda x: cos_values[x], reverse = True)[:10]
        return final_ten
    
    # define a tf model to calculate the cosine similarity
    def tf(self, query):
        cos_values = dict()
        self.query_tf = dict()
        doc_i = dict()
        term_doc = set()
        term_doc = self.index.keys()
        ids = self.doc_ids
        query_all_term = set()
        query_all_term = query
        # precompute term frequency of a query
        for term in query:
            self.query_tf[term] = 0
            if (self.query_tf[term] == 0):
                self.query_tf[term] += 1
        # match term in query and index
        for docid in ids:
            q_dot_d = 0
            for term in query_all_term:
                if term in term_doc and docid in self.index[term].keys():
                    query_i = self.query_tf[term]
                    doc_i = self.index[term][docid]
                    # # calculation of numerator
                    q_dot_d += query_i * doc_i
            cosine = q_dot_d / self.doc_size[docid]
            # recording cosine similarity values per docid
            cos_values[docid] = cosine
        # finished processing all the documents and returning top 10 ranked results
        final_ten = sorted(cos_values, key = lambda x: cos_values[x], reverse = True)[:10]
        return final_ten
    
    # define a tfidf model to calculate the cosine similarity
    def tfidf(self, query):
        cos_values = dict()
        self.query_tf = dict()
        term_doc = set()
        term_doc = self.index.keys()
        ids = self.doc_ids
        query_all_term = set()
        query_all_term = query
        # precompute term frequency of a query
        for term in query_all_term:
            self.query_tf[term] = 0
            if (self.query_tf[term] == 0):
                self.query_tf[term] += 1
        # match term in query and index
        for docid in ids:
            q_dot_d = 0
            for term in query_all_term:
                if term in term_doc and docid in self.index[term].keys():
                    query_i = self.query_tf[term] * self.idf_dict[term]
                    doc_i = self.index[term][docid] * self.idf_dict[term]
                    # calculation of numerator
                    q_dot_d += query_i * doc_i
            cosine = q_dot_d / self.doc_size[docid]
            # recording cosine similarity values per docid
            cos_values[docid] = cosine
        # finished processing all the documents and returning top 10 ranked results
        final_ten = sorted(cos_values, key = lambda x: cos_values[x], reverse = True)[:10]
        return final_ten

    def compute_number_of_documents(self):
        self.doc_ids = set()
        for term in self.index:
            self.doc_ids.update(self.index[term])
        return len(self.doc_ids)

    # call different functions to retrieve
    def for_query(self, query):
        if self.term_weighting == 'binary':
            result = self.binary(query)
        if self.term_weighting == 'tf':
            result = self.tf(query)
        if self.term_weighting == 'tfidf':
            result = self.tfidf(query)
        return result