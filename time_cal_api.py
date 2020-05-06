#from flask import Flask, request, redirect, url_for, flash, jsonify
#from flask_cors import CORS

import pandas as pd
import numpy as np

import win32com.client
import datetime
from dateutil.relativedelta import relativedelta

import pythoncom
import win32com.client as client

from datetime import timedelta
import datetime

import json

#app = Flask(__name__)
# CORS(app)


# THIS SHOULD COME FROM THE REACT FRONTEND - NOT BE HARDCODED!
END_WORK_TIME = "18:00"
START_WORK_TIME = "09:30"


def getCalendarEntries(from_date, to_date):
    pythoncom.CoInitialize()

    Outlook = win32com.client.Dispatch("Outlook.Application")
    ns = Outlook.GetNamespace("MAPI")
    appointments = ns.GetDefaultFolder(9).Items
    appointments.Sort("[Start]")
    appointments.IncludeRecurrences = "True"

    begin = datetime.datetime.strptime(
        from_date, "%d/%m/%Y").strftime("%d/%m/%Y")
    end = datetime.datetime.strptime(to_date, "%d/%m/%Y").strftime("%d/%m/%Y")

    appointments = appointments.Restrict(
        "[Start] >= '" + begin + "' AND [END] <= '" + end + "'")

    events = []
    for a in appointments:
        events.append({
            'Start': datetime.datetime.fromtimestamp(int(a.Start.timestamp())).strftime("%d/%m/%Y %H:%M"),
            'End': datetime.datetime.fromtimestamp(int(a.End.timestamp())).strftime("%d/%m/%Y %H:%M"),
            'Subject': a.Subject,
            'Duration': a.Duration,
            'AllDay': a.AllDayEvent})

    return pd.DataFrame (events)


def createOutofOffices(ndays, today, start_work_time, end_work_time):
    ooo_starts = []
    ooo_ends = []
    ooo_dur_mins = []

    yr = today.year
    mth = today.month
    dy = today.day

    xx = str(yr) + str(mth) + str(dy)
    xx_date = datetime.datetime.strptime(xx, "%Y%m%d")

    for _ in range(ndays):

        xx_wd = xx_date.weekday()

        #if xx_wd <= 4:
        if xx_wd == 4:
            new_cal_start = xx_date.strftime("%Y%m%d") + end_work_time #END_WORK_TIME
            new_cal_end = xx_date + datetime.timedelta(days=1)
            new_cal_end = new_cal_end.strftime("%Y%m%d")
            new_cal_end = datetime.datetime.strptime(
                new_cal_end + "00:00", "%Y%m%d%H:%M")
        if xx_wd == 5:
            new_cal_start = xx_date.strftime("%Y%m%d") + "00:01"
            new_cal_end = xx_date + datetime.timedelta(days=1)
            new_cal_end = new_cal_end.strftime("%Y%m%d")
            new_cal_end = datetime.datetime.strptime(
                new_cal_end + "00:00", "%Y%m%d%H:%M")
        if xx_wd == 6:
            new_cal_start = xx_date.strftime("%Y%m%d") + "00:01"
            new_cal_end = xx_date + datetime.timedelta(days=1)
            new_cal_end = new_cal_end.strftime("%Y%m%d")
            new_cal_end = datetime.datetime.strptime(
                new_cal_end + start_work_time, "%Y%m%d%H:%M")
        elif xx_wd < 4:
            new_cal_start = xx_date.strftime("%Y%m%d") + end_work_time #END_WORK_TIME
            new_cal_end = xx_date + datetime.timedelta(days=1)
            new_cal_end = new_cal_end.strftime("%Y%m%d")
            new_cal_end = datetime.datetime.strptime(
                new_cal_end + start_work_time, "%Y%m%d%H:%M")

        #new_cal_end = new_cal_end.strftime("%Y%m%d")
        #new_cal_end = datetime.datetime.strptime (new_cal_end + start_work_time, "%Y%m%d%H:%M")

        new_cal_start = datetime.datetime.strptime(
            new_cal_start, "%Y%m%d%H:%M")

        dur = new_cal_end - new_cal_start
        dur_mins = dur / datetime.timedelta(minutes=1)

        new_cal_end = new_cal_end.strftime("%Y/%m/%d %H:%M:%S")
        new_cal_start = new_cal_start.strftime("%Y/%m/%d %H:%M:%S")
        #new_cal_end = new_cal_end.strftime("%d/%m/%Y %H:%M")
        #new_cal_start = new_cal_start.strftime("%d/%m/%Y %H:%M")

        ooo_starts.append(new_cal_start)
        ooo_ends.append(new_cal_end)
        ooo_dur_mins.append(dur_mins)

        xx_date = xx_date + datetime.timedelta(days=1)

    ooos = pd.DataFrame({"Duration": ooo_dur_mins, "End": ooo_ends,
                         "Start": ooo_starts, "Subject": ["OOO"]*len(ooo_dur_mins)})
    ooos["AllDay"] = False

    return (ooos)