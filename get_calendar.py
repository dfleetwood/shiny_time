from O365 import Account
import pandas as pd
import datetime as dt

credentials = ('296ffada-6cf9-4795-87b7-1213e684022b', '?Zt5hO?]AGImtNQY@Bu20irHukP2uKP@')

def get_authorization_url():
    account = Account(credentials)
    url, state = account.con.get_authorization_url(requested_scopes = ['Calendars.Read'])
    account = Account(credentials)
    return ([url, state, account])

def complete_verification(req_url, state, account):
    result = account.con.request_token(req_url, 
                                   state=state)

def get_calendar (account):

    schedule = account.schedule()
    calendar = schedule.get_default_calendar()

    q = calendar.new_query('start').greater_equal(dt.datetime(2020, 4, 21))
    q.chain('and').on_attribute('end').less_equal(dt.datetime(2020, 7, 21))

    #calendar.get_events(query = q,limit = 999)

    events = []
    for event in calendar.get_events(query = q,limit = 9999):
        if (~event.is_all_day) & (~event.is_cancelled):
            events.append ({
            'start': event.start.strftime("%Y/%m/%d %H:%M:%S"),
            'end': event.end.strftime("%Y/%m/%d %H:%M:%S"),
            'name': event.subject,
            'id': event.ical_uid
            })
            

    return (pd.DataFrame (events))

