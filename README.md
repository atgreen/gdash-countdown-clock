# gdash-countdown-clock

gdash-countdown-clock is a simple web app presenting a countdown clock
for the next meeting scheduled in my Google calendar.  The calendar
agenda is polled via Google's API and published to an ActiveMQ topic.
See https://github.com/atgreen/gdash-gcal-poll for the trivial
implementation.  gdash-countdown-clock listens to that topic and
pushes the "Next Meeting" time into the browser via some AJAX magic.

Background colour changes to the webapp at the T-5 and T-2 minute
marks are implemented through CSS content.  The colour changes are
what really alerts users to upcoming meetings.

![Grafana panel example](/demo.gif?raw=true "grafana panel example")

I use gdash-countdown-clock as part of a personal grafana dashboard.
It is presented through a grafana HTML text panel with the following
content:

    <style>
    body {
        margin: 0;
    }
    .parent {
        display: flex;
        flex-direction: column;
        min-height: 100vh;
    }
    .parent .banner {
        background: #f00;
        width: 100%;
        height: 30px;
    }
    .parent iframe {
        background: #000;
        border: none;
        flex-grow: 1;
    }
    </style>
    
    <div class="parent">
        <iframe src="https://URI-FOR-MY-COUNTDOWN-CLOCK/"></iframe>
    </div>

I hope you find this interesting and useful.

AG
