# gdash-countdown-clock

gdash-countdown-clock is a simple web app presenting a countdown clock
to the next meeting scheduled in Google calendar.  The calendar agenda
is pulled down from Google by another project the polls Google's API
and publishes updates through an ActiveMQ topic.  See
https://github.com/atgreen/gdash-gcal-poll for the trivial
implementation.

gdash-countdown-clock listens to that topic and pushes the "Next
Meeting" time into the browser via some AJAX magic.

Background colour changes to the webapp at the T-5 and T-2 minute
marks are implemented through CSS content.

I use gdash-countdown-clock as part of a personal grafana dashboard.
This is done through a grafan HTML text panel with the following
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
