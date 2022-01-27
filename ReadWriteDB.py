'''
Created on 27 janvier 2021
@author:  cplumejeaud
Cours master 2 Python - ULR
Extract  some data from a relational database (postgres)
2 methods can be used : 
1. with pandas
2. with psycopg2

'''

# Librairies

import pandas.io.sql as sql
from sqlalchemy import create_engine 

import psycopg2
import sys, traceback

class ReadWriteDB(object):
    


    def __init__(self, config):
        if (config is not None):
            ## Open both a ssh connexion for copy/remove, and a tunnel for postgres connexion
            self.host = config.get('base', 'host')
            self.port = config.get('base', 'port')
            self.dbname = config.get('base', 'dbname')
            self.user = config.get('base', 'user')
            self.password = config.get('base', 'password')
        else :
            # Change this in 
            self.host = 'localhost'
            self.port = '5432'
            self.dbname = 'pameli'
            self.user = 'postgres'
            self.password = 'postgres'

        self.postgresconn = self.open_connection()

    def read_with_pandas(self, query):
        '''
        Example : 
        select context_id, reason, date, sensor_name, property_name, property_value,
        unit_code, unit_name, point_id, st_x(st_transform(geom_point, 4326)) as pt_longitude, st_y(st_transform(geom_point, 4326)) as pt_latitude, 
        st_transform(geom_point, 4326) as geom_point, local_time, is_reference 
        from flat_data where context_id=1 and sensor_name = 'ysiexo2'

        :param query: query looks like a string with this content :  SELECT something FROM somewhere WHERE filters 
        :return: the result of the query as a dataframe, with columns having same names as attributes in the something list
        '''
        engine = create_engine('postgresql://'+self.user+':'+self.password+'@'+self.host+':'+self.port+'/'+self.dbname)
        print(engine)


        data = sql.read_sql_query(query, engine)
        return data

    def write_with_pandas(self, dataframe):
        '''
        :param dataframe: a dataframe to store as a table 
        '''
        engine = create_engine('postgresql://'+self.user+':'+self.password+'@'+self.host+':'+self.port+'/'+self.dbname)
        print(engine)

        tablename = 'name_of_the_table'
        #look at the doc to either append or not data : 'fails', 'replace', 'append'
        #https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.to_sql.html
        dataframe.to_sql(dataframe, con=engine, if_exists='append', method='multi')

    def close_connection(self):
        '''
        Cleanly close DB connection
        :param postgresconn:
        :return:
        '''
        if self.postgresconn is not None:
            self.postgresconn.close()

    def open_connection(self):
        '''
        Open database connection with Postgres
        :param config:
        :return: a valid connection to DB
        '''
        driverPostgres = 'host=' + self.host + ' port=' + self.port + ' user=' + self.user + ' dbname=' + self.dbname + ' password=' + self.password

        conn = None
        try:
            conn = psycopg2.connect(driverPostgres)
        except Exception as e:
            print("I am unable to connect to the database. " + str(e))
        # Test DB
        if conn is not None:
            cur = conn.cursor()
            cur.execute('select count(*) from pg_namespace')
            result = cur.fetchone()
            if result is None:
                print('open_connection Failed to get count / use of database failed')
            else:
                print('open_connection Got database connexion : ' + str(result[0]))
        else:
            print('open_connection Failed to get database connexion')

        return conn

    def execute_sql(self, sql_query):
        '''
        :sql_query: query can be an UPDATE or INSERT or DELETE command
        '''
    
        cur = self.postgresconn.cursor()
        try:
            cur.execute(sql_query)
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            print(e)
            print(repr(traceback.format_exception(exc_type, exc_value, exc_traceback)))

        cur.close()
        self.postgresconn.commit()

    def select_sql(self, sql_query):
        '''
        Example : 
        select context_id, reason, date, sensor_name, property_name, property_value,
        unit_code, unit_name, point_id, st_x(st_transform(geom_point, 4326)) as pt_longitude, st_y(st_transform(geom_point, 4326)) as pt_latitude, 
        st_transform(geom_point, 4326) as geom_point, local_time, is_reference 
        from flat_data where context_id=1 and sensor_name = 'ysiexo2'

        :sql_query: query is a SELECT command : SELECT col1, col2, ..., colp FROM atable WHERE afilter GROUP BY acol HAVING acondition
        :return: a matrix of n rows and p columns on which you can iterate.
        '''
        cur = self.postgresconn.cursor()
        try:
            cur.execute(sql_query)
            return cur.fetchall()
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            print(e)
            print(repr(traceback.format_exception(exc_type, exc_value, exc_traceback)))

        cur.close()
