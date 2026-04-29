#!/usr/bin/env python3
#
# Generate conference calendar in ICAL format.
#
# Depends on:
# * icalendar

import json
from datetime import date, datetime, time, timedelta
from functools import cached_property
from pathlib import Path
from zoneinfo import ZoneInfo

from icalendar import Alarm, Calendar, Event


def load_json(path):
    with path.open() as file:
        return json.load(file)


class CalendarGenerator:
    DATA = Path("data")
    TIMEZONE = ZoneInfo("Europe/Stockholm")

    def __init__(self):
        self.cal = Calendar()
        self.cal.add("version", "1.0")
        self.cal.add("prodid", "-//Chalmers University of Technology//TYPES 2026//EN")
        self.cal.add("summary", "TYPES 2026")

    @cached_property
    def inviteds(self):
        return load_json(self.DATA / "invited.json")

    @cached_property
    def sessions(self):
        return {
            session["id"]: session for session in load_json(self.DATA / "sessions.json")
        }

    @cached_property
    def papers(self):
        return {paper["pid"]: paper for paper in load_json(self.DATA / "papers.json")}

    @cached_property
    def schedule(self):
        return load_json(self.DATA / "schedule.json")

    def parse_datetime(self, date_str, time_str) -> datetime:
        return datetime.combine(
            date.fromisoformat(date_str),
            time.fromisoformat(time_str),
            tzinfo=self.TIMEZONE,
        )

    def authors(self, paper):
        for author in paper["authors"]:
            yield " ".join([author["first"], author["last"]])

    def uid(self, identifier: str):
        return identifier + "@types2026.cse.chalmers.se"

    def process_event(self, time_from, time_to, event):
        LENGTH_CONTRIBUTED_TALK = timedelta(minutes=20)
        LOCATION_CONFERENCE = "Lindholmen Conference Centre"
        LOCATION_TALK = "Lindholmen Conference Centre, Room Pascal"
        ALARM_DISABLE = date(year=1984, month=1, day=1)

        type_ = event["type"]
        title = event.get("title")

        def add(
            identifier=None,
            start=time_from,
            end=time_to,
            summary=title,
            description=None,
            location=None,
            trigger=None,
        ):
            if identifier is None:
                identifier = summary.lower().replace(" ", "_")
            identifier = f"{type_}_{identifier}"
            event = Event()
            event.add("uid", self.uid(identifier))
            event.add("summary", summary)
            event.add("dtstart", start)
            event.add("dtend", end)
            if location is not None:
                event.add("location", location)
            if description is not None:
                event.add("description", description)
            if trigger is not None:
                alarm = Alarm()
                alarm.add("action", "DISPLAY")
                alarm.add("trigger", trigger)
                event.add_component(alarm)
            self.cal.add_component(event)

        match type_:
            case "break":
                if title == "Registration":
                    add(location=LOCATION_CONFERENCE)
            case "special":

                match title:
                    case "Dinner":
                        add(location="Wijkanders, Vera Sandersbergs allé 5B")
                    case "Excursion":
                        add(location="Lindholmenspiren", trigger=timedelta(minutes=-10))
                    case "Opening address":
                        add(location=LOCATION_TALK)
                    case "Business meeting":
                        add(location=LOCATION_TALK, trigger=timedelta(minutes=-5))
            case "invited_talk":
                speaker = event["speaker"]
                invited = self.inviteds[speaker]
                add(
                    identifier=speaker,
                    summary=invited["title"],
                    description=invited["speaker"],
                    location=LOCATION_TALK,
                )
            case "session":
                session = self.sessions[event["id"]]
                time_start = time_from
                trigger = timedelta(minutes=-5)
                for pid in session["papers"]:
                    paper = self.papers[pid]

                    time_end = time_start + LENGTH_CONTRIBUTED_TALK
                    add(
                        identifier=str(pid),
                        start=time_start,
                        end=time_end,
                        summary=paper["title"],
                        description=", ".join(self.authors(paper)),
                        location=LOCATION_TALK,
                        trigger=trigger,
                    )
                    time_start = time_end
                    trigger = ALARM_DISABLE

    def generate(self):
        for schedule_entry in self.schedule:
            for event in schedule_entry["events"]:
                self.process_event(
                    self.parse_datetime(schedule_entry["date"], event["from"]),
                    self.parse_datetime(schedule_entry["date"], event["to"]),
                    event,
                )

    def format(self):
        return self.cal.to_ical()


gen = CalendarGenerator()
gen.generate()
print(gen.format().decode())
