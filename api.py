import requests
from requests.compat import urljoin

class Problem(object):
    """A real problem instance given by the API."""

    def __init__(self, json):
        self.id = json['id']
        self.size = int(json['size'])
        self.operators = json['operators']
        self.solved = True if 'solved' in json else False
        self.time_left = int(json['timeLeft?']) if 'timeLeft?' in json else 300

class TrainingProblem(object):
    """A training problem instance given by the API."""

    def __init__(self, json):
        self.challenge = json['challenge']
        self.id = json['id']
        self.size = int(json['size'])
        self.operators = json['operators']

    def __str__(self):
        return self.challenge

class Client(object):
    """Issues requests to the ICFP 2013 API.

    Must be provided a team-specific authentication token.
    Methods may throw a variety of exception types, as
    defined below.
    """

    BASE_URL = 'http://icfpc2013.cloudapp.net/'

    def __init__(self, auth):
        self.auth = auth + 'vpsH1H'

    def myproblems(self):
        """Issue the 'myproblems' request; return a list of my problems."""
        return self._post('myproblems')

    def train(self, size=None, operators=None):
        """Issue the 'train' request; return a training problem."""
        if operators not in [None, '', 'tfold', 'fold']:
            raise APIError, "Invalid operators value."

        payload = {}
        if size: payload['size?'] = size
        if operators: payload['operators?'] = operators

        return TrainingProblem(self._post('train', payload))

    def _get(self, path):
        """Perform a GET request to a path; return decoded JSON."""
        url = urljoin(Client.BASE_URL, path)
        resp = requests.get(url, params={'auth' : self.auth})
        if not resp.ok:
            raise APIException, resp.status_code
        return resp.json()

    def _post(self, path, payload=None):
        """Perform a POST request to a path; return decoded JSON."""
        url = urljoin(Client.BASE_URL, path)
        resp = requests.post(url, params={'auth' : self.auth}, data=payload)
        if not resp.ok:
            raise APIException, resp.status_code
        return resp.json()
        

class APIException(Exception):
    """An error was returned from the API."""
    pass

# This is our *real* API key
c = Client('0417IVU3bbugkcfbldUEEuBw4LfbMgFyHtF84Qyf')
#c = Client(auth='0000abcdefghijklmnopqrstuvwxyz0123456789vpsH1H')
print c.train()
