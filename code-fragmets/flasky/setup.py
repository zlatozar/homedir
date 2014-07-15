# -*- coding: utf-8 -*-

from setuptools import setup

project = "project name"

setup(
    name=project,
    version='0.1',
    url='Project URL',
    description='Project description',
    author='your name',
    author_email='your_email@example.com',
    packages=["project name"],
    include_package_data=True,
    zip_safe=False,
    install_requires=[
        'Flask',
        'Flask-Babel',
        'Flask-Bootstrap',
        'Flask-DebugToolbar',
        'Flask-HTTPAuth',
        'Flask-Login',
        'Flask-Mail',
        'Flask-Migrate',
        'Flask-Moment',
        'Flask-OAuth',
        'Flask-RESTful',
        'Flask-SQLAlchemy',
        'Flask-Script',
        'Flask-WTF',
        'celery',
    ],
    test_suite='tests',
    classifiers=[
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Internet :: WWW/HTTP :: Dynamic Content',
        'Topic :: Software Development :: Libraries'
    ]
)
